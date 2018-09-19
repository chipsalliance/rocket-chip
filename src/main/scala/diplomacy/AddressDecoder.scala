// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel.log2Ceil
import scala.math.{max,min}

import freechips.rocketchip.util.CacheCompute

object AddressDecoder
{
  private def time[R](name: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    val timeMillis = (t1 - t0) / 1000000.0
    println(s" ***** $name took $timeMillis ms *****")
    result
  }

  type Port = Seq[AddressSet]
  type Ports = Seq[Port]
  type Partition = Ports
  type Partitions = Seq[Partition]

  val addressOrder = Ordering.ordered[AddressSet]
  val portOrder = Ordering.Iterable(addressOrder)
  val partitionOrder = Ordering.Iterable(portOrder)

  // Find the minimum subset of bits needed to disambiguate port addresses.
  // ie: inspecting only the bits in the output, you can look at an address
  //     and decide to which port (outer Seq) the address belongs.
  def apply(ports: Ports, givenBits: BigInt = BigInt(0)): BigInt = {
    val nonEmptyPorts = ports.filter(_.nonEmpty)
    if (nonEmptyPorts.size <= 1) {
      givenBits
    } else {
      // Verify the user did not give us an impossible problem
      nonEmptyPorts.combinations(2).foreach { case Seq(x, y) =>
        x.foreach { a => y.foreach { b =>
          require (!a.overlaps(b), s"Ports cannot overlap: $a $b")
        } }
      }
      CacheCompute((ports, givenBits), applyImpl(ports, givenBits))
    }
  }
  def applyImpl(ports: Ports, givenBits: BigInt = BigInt(0)): BigInt = {
    val nonEmptyPorts = ports.filter(_.nonEmpty)
      val maxBits = log2Ceil(1 + nonEmptyPorts.map(_.map(_.base).max).max)
      val (bitsToTry, bitsToTake) = (0 until maxBits).map(BigInt(1) << _).partition(b => (givenBits & b) == 0)
      val partitions = Seq(nonEmptyPorts.map(_.sorted).sorted(portOrder))
      val givenPartitions = bitsToTake.foldLeft(partitions) { (p, b) => partitionPartitions(p, b) }
      val selected = time("recurse")(recurse(givenPartitions, bitsToTry.reverse.toSeq))
      val output = selected.reduceLeft(_ | _) | givenBits

      // Modify the AddressSets to allow the new wider match functions
      val widePorts = nonEmptyPorts.map { _.map { _.widen(~output) } }
      // Verify that it remains possible to disambiguate all ports
      widePorts.combinations(2).foreach { case Seq(x, y) =>
        x.foreach { a => y.foreach { b =>
          require (!a.overlaps(b), s"Ports cannot overlap: $a $b")
        } }
      }
      //System.exit(0)

      output
  }

  // A simpler version that works for a Seq[Int]
  def apply(keys: Seq[Int]): Int = {
    val ports = keys.map(b => Seq(AddressSet(b, 0)))
    apply(ports).toInt
  }

  // The algorithm has a set of partitions, discriminated by the selected bits.
  // Each partion has a set of ports, listing all addresses that lead to that port.
  // Seq[Seq[Seq[AddressSet]]]
  //         ^^^^^^^^^^^^^^^ set of addresses that are routed out this port
  //     ^^^ the list of ports
  // ^^^ cases already distinguished by the selected bits thus far
  //
  // Solving this problem is NP-hard, so we use a simple greedy heuristic:
  //   pick the bit which minimizes the number of ports in each partition
  //   as a secondary goal, reduce the number of AddressSets within a partition

  def bitScore(partitions: Partitions): Seq[Int] = {
    val maxPortsPerPartition = partitions.map(_.size).max
    val maxSetsPerPartition = partitions.map(_.map(_.size).sum).max
    val sumSquarePortsPerPartition = partitions.map(p => p.size * p.size).sum
    val sumSquareSetsPerPartition = partitions.map(_.map(p => p.size * p.size).sum).max
    Seq(maxPortsPerPartition, maxSetsPerPartition, sumSquarePortsPerPartition, sumSquareSetsPerPartition)
  }

  def partitionPort(port: Port, bit: BigInt): (Port, Port) = {
    val addr_a = AddressSet(0, ~bit)
    val addr_b = AddressSet(bit, ~bit)
    // The addresses were sorted, so the filtered addresses are still sorted
    val subset_a = port.filter(_.overlaps(addr_a))
    val subset_b = port.filter(_.overlaps(addr_b))
    (subset_a, subset_b)
  }

  def partitionPorts(ports: Ports, bit: BigInt): (Ports, Ports) = {
    val partitioned_ports = ports.map(p => partitionPort(p, bit))
    // because partitionPort dropped AddresSets, the ports might no longer be sorted
    val case_a_ports = partitioned_ports.map(_._1).filter(!_.isEmpty).sorted(portOrder)
    val case_b_ports = partitioned_ports.map(_._2).filter(!_.isEmpty).sorted(portOrder)
    (case_a_ports, case_b_ports)
  }
  
  def partitionPartitions(partitions: Partitions, bit: BigInt): Partitions = {
    val partitioned_partitions = partitions.map(p => partitionPorts(p, bit))
    val case_a_partitions = partitioned_partitions.map(_._1).filter(!_.isEmpty)
    val case_b_partitions = partitioned_partitions.map(_._2).filter(!_.isEmpty)
    val new_partitions = (case_a_partitions ++ case_b_partitions).sorted(partitionOrder)
    // Prevent combinational memory explosion; if two partitions are equal, keep only one
    // Note: AddressSets in a port are sorted, and ports in a partition are sorted.
    // This makes it easy to structurally compare two partitions for equality
    val keep = (new_partitions.init zip new_partitions.tail) filter { case (a,b) => partitionOrder.compare(a,b) != 0 } map { _._2 }
    new_partitions.head +: keep
  }

  // requirement: ports have sorted addresses and are sorted lexicographically
  val debug = false
  def recurse(partitions: Partitions, bits: Seq[BigInt]): Seq[BigInt] = {
    if (partitions.map(_.size <= 1).reduce(_ && _)) Seq() else {
      if (debug) {
        println("Partitioning:")
        partitions.foreach { partition =>
          println("  Partition:")
          partition.foreach { port =>
            print("   ")
            port.foreach { a => print(s" ${a}") }
            println("")
          }
        }
      }
      val candidates = bits.map { bit =>
        val result = partitionPartitions(partitions, bit)
        val score = bitScore(result)
        if (debug)
          println("  For bit %x, %s".format(bit, score.toString))
        (score, bit, result)
      }
      val (bestScore, bestBit, bestPartitions) = candidates.min(Ordering.by[(Seq[Int], BigInt, Partitions), Iterable[Int]](_._1.toIterable))
      if (debug) println("=> Selected bit 0x%x".format(bestBit))
      bestBit +: recurse(bestPartitions, bits.filter(_ != bestBit))
    }
  }
}

object JackTest extends App {
  val xs = List(List(AddressSet(0x10000L, 0x7fffL), AddressSet(0x1000L, 0xfffL),
    AddressSet(0x20000000L, 0xfffffffL), AddressSet(0x30000000L, 0xfffffffL)),
    List(AddressSet(0x100b8000L, 0xfffL), AddressSet(0x2010000L, 0xfffL),
    AddressSet(0xc000000L, 0x3ffffffL), AddressSet(0x2000000L, 0xffffL),
    AddressSet(0x0L, 0xfffL), AddressSet(0x1800000L, 0x3fffL),
    AddressSet(0x1000000L, 0x1fffL),
    AddressSet(0x1700000L, 0xfffL), AddressSet(0x1808000L, 0x7fffL),
    AddressSet(0x1701000L, 0xfffL), AddressSet(0x1810000L, 0x7fffL),
    AddressSet(0x1702000L, 0xfffL), AddressSet(0x1818000L, 0x7fffL),
    AddressSet(0x1703000L, 0xfffL), AddressSet(0x1820000L, 0x7fffL),
    AddressSet(0x1704000L, 0xfffL), AddressSet(0x4000L, 0xfffL),
    AddressSet(0x3000000L, 0xfffffL),
    AddressSet(0x100b0000L, 0x3fffL), AddressSet(0x100c0000L, 0xfffL),
    AddressSet(0x10100000L, 0xfffL), AddressSet(0x2020000L, 0xfffL),
    AddressSet(0x10070000L, 0xfffL), AddressSet(0x10000000L, 0xfffL),
    AddressSet(0x10090000L, 0x1fffL), AddressSet(0x100a0000L, 0xfffL),
    AddressSet(0x10080000L, 0xfffL), AddressSet(0x10010000L, 0xfffL),
    AddressSet(0x10011000L, 0xfffL), AddressSet(0x10050000L, 0xfffL),
    AddressSet(0x10060000L, 0xfffL), AddressSet(0x10020000L, 0xfffL),
    AddressSet(0x10021000L, 0xfffL), AddressSet(0x10030000L, 0xfffL),
    AddressSet(0x10040000L, 0xfffL), AddressSet(0x10041000L, 0xfffL),
    AddressSet(0x8000000L, 0x1fffffL), AddressSet(0xa000000L, 0x1ffff3fL),
    AddressSet(0x80000000L, 0x7fffff3fL), AddressSet(0x100000000L, 0xffffff3fL),
    AddressSet(0x200000000L, 0x1ffffff3fL), AddressSet(0x400000000L, 0x3ffffff3fL),
    AddressSet(0x800000000L, 0x7ffffff3fL), AddressSet(0x1000000000L, 0xfffffff3fL),
    AddressSet(0x60000000L, 0x1fffff3fL), AddressSet(0x3000000000L, 0xfffffff3fL),
    AddressSet(0xa000040L, 0x1ffff3fL), AddressSet(0x80000040L, 0x7fffff3fL),
    AddressSet(0x100000040L, 0xffffff3fL), AddressSet(0x200000040L, 0x1ffffff3fL),
    AddressSet(0x400000040L, 0x3ffffff3fL), AddressSet(0x800000040L, 0x7ffffff3fL),
    AddressSet(0x1000000040L, 0xfffffff3fL), AddressSet(0x60000040L, 0x1fffff3fL),
    AddressSet(0x3000000040L, 0xfffffff3fL), AddressSet(0xa000080L, 0x1ffff3fL),
    AddressSet(0x80000080L, 0x7fffff3fL), AddressSet(0x100000080L, 0xffffff3fL),
    AddressSet(0x200000080L, 0x1ffffff3fL), AddressSet(0x400000080L, 0x3ffffff3fL),
    AddressSet(0x800000080L, 0x7ffffff3fL), AddressSet(0x1000000080L, 0xfffffff3fL),
    AddressSet(0x60000080L, 0x1fffff3fL), AddressSet(0x3000000080L, 0xfffffff3fL),
    AddressSet(0xa0000c0L, 0x1ffff3fL), AddressSet(0x800000c0L, 0x7fffff3fL),
    AddressSet(0x1000000c0L, 0xffffff3fL), AddressSet(0x2000000c0L, 0x1ffffff3fL),
    AddressSet(0x4000000c0L, 0x3ffffff3fL), AddressSet(0x8000000c0L, 0x7ffffff3fL),
    AddressSet(0x10000000c0L, 0xfffffff3fL), AddressSet(0x600000c0L, 0x1fffff3fL),
    AddressSet(0x30000000c0L, 0xfffffff3fL), AddressSet(0x18000000L, 0x7ffffffL),
    AddressSet(0x40000000L, 0x1fffffffL), AddressSet(0x2000000000L, 0xfffffffffL))
  )
  CacheCompute(xs, AddressDecoder(xs))
  CacheCompute(xs, AddressDecoder(xs))
  CacheCompute(xs, AddressDecoder(xs))
}
