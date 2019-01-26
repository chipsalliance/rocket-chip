// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import freechips.rocketchip.config.Field
import sys.process._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

case object DTSModel    extends Field[String]
case object DTSCompat   extends Field[Seq[String]] // -dev, -soc
case object DTSTimebase extends Field[BigInt](0)   // Clock frequency of clint RTC (use 0 if you don't know it)

object DTS
{
  def apply(res: ResourceValue): String = "/dts-v1/;\n\n" + helper(res, "", defaultCells).mkString("")

  private val nodeStartChars = (('a' to 'z') ++ ('A' to 'Z')).toSet
  private val nodeChars = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Seq(',', '.', '_', '+', '-', '@')).toSet
  def legalNode(x: String): Boolean =
    x == "/" || (!x.isEmpty && x.size < 32 && nodeStartChars.contains(x(0)) && x.forall(nodeChars.contains(_)))

  // the DTS spec does not list '-', but uses it everywhere ...
  private val propChars = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Seq(',', '.', '_', '+', '?', '#', '-')).toSet
  def legalProperty(x: String): Boolean =
    x == "/" || (!x.isEmpty && x.size < 32 && x.forall(propChars.contains(_)))

  // The DTS spec doesn't say what is allowed in a string, so just use our own judgement
  private val strChars = (('#' to '[') ++ (']' to '~') ++ Seq(' ', '!')).toSet
  def legalString(x: String): Boolean = x.forall(strChars.contains(_))

  private case class Cells(
    parentAddress: Int,
    parentSize:    Int,
    selfAddress:   Int,
    selfSize:      Int)
  private val defaultCells = Cells(2, 1, 2, 1)

  private def fmtCell(x: BigInt, cells: Int): Seq[String] = {
    val cellbits = 32
    val mask = (BigInt(1) << cellbits) - 1
    (0 until cells).reverse map { case i => "0x%x".format((x >> (i*cellbits)) & mask) }
  }

  private def fmtAddress(x: ResourceAddress, cells: Cells): Seq[String] = {
    val ranges = AddressRange.fromSets(x.address)
    ranges.flatMap { case AddressRange(base, size) =>
      fmtCell(base, cells.parentAddress)  ++ fmtCell(size, cells.parentSize)
    }
  }

  private def fmtMapping(x: ResourceMapping, cells: Cells): Seq[String] = {
    val ranges = AddressRange.fromSets(x.address)
    ranges.flatMap { case AddressRange(base, size) =>
      fmtCell(base+x.offset, cells.selfAddress) ++ fmtCell(base, cells.parentAddress) ++ fmtCell(size, cells.selfSize)
    }
  }

  private def fmtString(x: ResourceString): Seq[String] = {
    require (legalString(x.value), s"The string '${x.value}' contains chars probably unwise for use in a DTS string")
    Seq("\"" + x.value + "\"")
  }

  private def fmtMap(x: ResourceMap, indent: String, cells: Cells): Seq[String] = {
    val (nodes, props) = x.value.partition(_ match {
      case (_, Seq(ResourceMap(_, _))) => true
      case _ => false
    })

    def getInt(x: ResourceValue) = x match {
      case ResourceInt(value) => Some(value.toInt)
      case _ => None
    }
    val selfAddress = x.value.getOrElse("#address-cells", Nil).headOption.flatMap(getInt)
    val selfSize    = x.value.getOrElse("#size-cells",    Nil).headOption.flatMap(getInt)

    val myCells = Cells(
      parentAddress = cells.selfAddress,
      parentSize    = cells.selfSize,
      selfAddress   = selfAddress.getOrElse(defaultCells.selfAddress),
      selfSize      = selfSize   .getOrElse(defaultCells.selfSize))

    props.flatMap { case (k, seq) =>
      require (legalProperty(k), s"The string '${k}' is not a legal DTS property name")
      seq.headOption match {
        case None => Seq(indent, k, ";\n")
        case Some(ResourceString(_)) => {
          seq.foreach { r => r match {
            case ResourceString(_) => Unit
            case _ => require(false, s"The property '${k}' has values of conflicting type: ${seq}")
          } }
          Seq(indent, k, " = ", seq.flatMap(z => helper(z, "", myCells)).mkString(", "), ";\n")
        }
        case Some(ResourceAlias(_)) => {
          seq.foreach { r => r match {
            case ResourceAlias(_) => Unit
            case _ => require(false, s"The property '${k}' has values of conflicting type: ${seq}")
          } }
          Seq(indent, k, " = ", seq.flatMap(z => helper(z, "", myCells)).mkString(", "), ";\n")
        }
        case Some(_) => {
          seq.foreach { r => r match {
            case ResourceMap(_, _) => require(false, s"The property '${k}' has values of conflicting type: ${seq}")
            case ResourceString(_) => require(false, s"The property '${k}' has values of conflicting type: ${seq}")
            case ResourceAlias(_)  => require(false, s"The property '${k}' has values of conflicting type: ${seq}")
            case _ => Unit
          } }
          Seq(indent, k, " = <", seq.flatMap(z => helper(z, "", myCells)).mkString(" "), ">;\n")
        }
      }
    }.toList ++
    nodes.flatMap { case (k, Seq(s: ResourceMap)) =>
      require (legalNode(k), s"The string '${k}' is not a legal DTS node name")
      Seq(indent) ++ s.labels.map(_ + ": ").filter(_ => !indent.isEmpty) ++ // labels on root not allowed
      Seq(k, " {\n") ++ helper(s, indent + "\t", myCells) ++ Seq(indent, "};\n")
    }
  }

  private def helper(res: ResourceValue, indent: String, cells: Cells): Seq[String] = res match {
    case x: ResourceAddress => fmtAddress(x, cells)
    case x: ResourceMapping => fmtMapping(x, cells)
    case x: ResourceInt => Seq(x.value.toString)
    case x: ResourceString => fmtString(x)
    case x: ResourceReference => Seq("&" + x.value)
    case x: ResourceAlias => Seq("&" + x.value)
    case x: ResourceMap => fmtMap(x, indent, cells)
  }
}

case class DTB(contents: Seq[Byte])
object DTB
{
  def apply(dts: String): DTB = {
    val instream = new ByteArrayInputStream(dts.getBytes("UTF-8"))
    val outstream = new ByteArrayOutputStream
    val proc = "dtc -O dtb" #< instream #> outstream
    require (proc.! == 0, "Failed to run dtc; is it in your path?")
    instream.close
    outstream.close
    DTB(outstream.toByteArray)
  }
}
