// See LICENSE.SiFive for license details.

package rocket

import Chisel._
import diplomacy._
import uncore.tilelink2._

case class TLBPermissions(
  homogeneous: Bool, // if false, the below are undefined
  r: Bool, // readable
  w: Bool, // writeable
  x: Bool, // executable
  c: Bool, // cacheable
  a: Bool, // arithmetic ops
  l: Bool) // logical ops

object TLBPageLookup
{
  private case class TLBFixedPermissions(
    r: Boolean, // readable
    w: Boolean, // writeable
    x: Boolean, // executable
    c: Boolean, // cacheable
    a: Boolean, // arithmetic ops
    l: Boolean) { // logical ops
    val useful = r || w || x || c || a || l
  }

  // Unmapped memory is considered to be inhomogeneous
  def apply(managers: Seq[TLManagerParameters], xLen: Int, cacheBlockBytes: Int, pageSize: BigInt): UInt => TLBPermissions = {
    require (isPow2(xLen) && xLen >= 8)
    require (isPow2(cacheBlockBytes) && cacheBlockBytes >= xLen/8)
    require (isPow2(pageSize) && pageSize >= cacheBlockBytes)

    val xferSizes = TransferSizes(cacheBlockBytes, cacheBlockBytes)
    val allSizes = TransferSizes(1, cacheBlockBytes)
    val amoSizes = TransferSizes(4, xLen/8)

    val permissions = managers.map { m =>
      require (!m.supportsGet        || m.supportsGet       .contains(allSizes),  s"MemoryMap region ${m.name} only supports ${m.supportsGet} Get, but must support ${allSizes}")
      require (!m.supportsPutFull    || m.supportsPutFull   .contains(allSizes),  s"MemoryMap region ${m.name} only supports ${m.supportsPutFull} PutFull, but must support ${allSizes}")
      require (!m.supportsAcquireB   || m.supportsAcquireB  .contains(xferSizes), s"MemoryMap region ${m.name} only supports ${m.supportsAcquireB} AcquireB, but must support ${xferSizes}")
      require (!m.supportsAcquireT   || m.supportsAcquireT  .contains(xferSizes), s"MemoryMap region ${m.name} only supports ${m.supportsAcquireT} AcquireT, but must support ${xferSizes}")
      require (!m.supportsLogical    || m.supportsLogical   .contains(amoSizes),  s"MemoryMap region ${m.name} only supports ${m.supportsLogical} Logical, but must support ${amoSizes}")
      require (!m.supportsArithmetic || m.supportsArithmetic.contains(amoSizes),  s"MemoryMap region ${m.name} only supports ${m.supportsArithmetic} Arithmetic, but must support ${amoSizes}")
      require (m.supportsAcquireT || !m.supportsAcquireB, s"MemoryMap region ${m.name} supports AcquireB (cached read) but not AcquireT (cached write)... and rocket assumes this")

      (m.address, TLBFixedPermissions(
        r = m.supportsGet     || m.supportsAcquireB, // if cached, never uses Get
        w = m.supportsPutFull || m.supportsAcquireT, // if cached, never uses Put
        x = m.executable,
        c = m.supportsAcquireB,
        a = m.supportsArithmetic,
        l = m.supportsLogical))
    }

    val grouped: Map[TLBFixedPermissions, Seq[AddressSet]] = permissions
      .filter(_._2.useful) // get rid of no-permission devices
      .groupBy(_._2) // group by permission type
      .mapValues(seq => 
        AddressSet.unify(seq.flatMap(_._1)) // coalesce same-permission regions
        .filter(_.alignment >= pageSize)) // discard any region that's not big enough

    def lowCostProperty(prop: TLBFixedPermissions => Boolean): UInt => Bool = {
      val (yesm, nom) = grouped.partition { case (k, eq) => prop(k) }
      val (yes, no) = (yesm.values.flatten.toList, nom.values.flatten.toList)
      // Find the minimal bits needed to distinguish between yes and no
      val decisionMask = AddressDecoder(Seq(yes, no))
      def simplify(x: Seq[AddressSet]) = AddressSet.unify(x.map(_.widen(~decisionMask)).distinct)
      val (yesf, nof) = (simplify(yes), simplify(no))
      if (yesf.size < no.size) {
        (x: UInt) => yesf.map(_.contains(x)).foldLeft(false.B)(_ || _)
      } else {
        (x: UInt) => !nof.map(_.contains(x)).foldLeft(false.B)(_ || _)
      }
    }

    // Derive simplified property circuits (don't care when !homo)
    val rfn = lowCostProperty(_.r)
    val wfn = lowCostProperty(_.w)
    val xfn = lowCostProperty(_.x)
    val cfn = lowCostProperty(_.c)
    val afn = lowCostProperty(_.a)
    val lfn = lowCostProperty(_.l)

    val homo = AddressSet.unify(grouped.values.flatten.toList)
    (x: UInt) => TLBPermissions(
      homogeneous = homo.map(_.contains(x)).foldLeft(false.B)(_ || _),
      r = rfn(x),
      w = wfn(x),
      x = xfn(x),
      c = cfn(x),
      a = afn(x),
      l = lfn(x))
  }
}
