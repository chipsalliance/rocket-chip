// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel

import java.io.{File, FileWriter}
import java.lang.management.OperatingSystemMXBean

import freechips.rocketchip.diplomacy.DTS.{Cells, fmtCell}
import freechips.rocketchip.diplomacy.{AddressRange, Binding, Device, ResourceAddress, ResourceAlias, ResourceBindings, ResourceInt, ResourceMap, ResourceMapping, ResourcePermissions, ResourceReference, ResourceString, ResourceValue}
import freechips.rocketchip.diplomaticobjectmodel.model._
import org.json4s.jackson.JsonMethods.pretty
import org.json4s.jackson.Serialization
import org.json4s.{Extraction, NoTypeHints}


object DiplomaticObjectModelUtils {

  def toJson(json: Any): String = {
    implicit val formats = Serialization.formats(NoTypeHints)
    pretty(Extraction.decompose(json))
  }

  def writeJsonFile(filename: String, json: Map[String, Any]): Unit = {
    val writer = new FileWriter(new File(filename))
    writer.write(toJson(json))
    writer.close()
  }
}

object DiplomaticObjectModelAddressing {

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

  private def omPerms(p: ResourcePermissions): OMPermissions = {
    OMPermissions(
      readable = p.r,
      writeable = p.w,
      executable = p.x,
      cacheable = p.c,
      atomics = p.a
    )
  }

  private def fmtAddress(x: ResourceAddress, cells: Cells): Seq[String] = {
    val ranges = AddressRange.fromSets(x.address)
    ranges.flatMap { case AddressRange(base, size) =>
      fmtCell(base, cells.parentAddress)  ++ fmtCell(size, cells.parentSize)
    }
  }

  private def omAddressSets(ranges: Seq[AddressRange]): Seq[OMAddressSet] = {
    ranges.map {
      case AddressRange(base, size) =>
        OMAddressSet(base = base, mask = size - 1)
    }
  }

  private def omAddress(x: ResourceAddress): Seq[OMAddressSet] = {
    val ranges = AddressRange.fromSets(x.address)
    omAddressSets(ranges)
  }

  private def fmtMapping(x: ResourceMapping, cells: Cells): Seq[String] = {
    val ranges = AddressRange.fromSets(x.address)
    ranges.flatMap { case AddressRange(base, size) =>
      fmtCell(base+x.offset, cells.selfAddress) ++ fmtCell(base, cells.parentAddress) ++ fmtCell(size, cells.selfSize)
    }
  }

  private def omMemoryRegion(name: String, value: ResourceValue): OMMemoryRegion = {
    val (omRanges, permissions) = value match {
      case rm: ResourceMapping =>
        val ranges = AddressRange.fromSets(rm.address)
        (omAddressSets(ranges),rm.permissions)
      case ra: ResourceAddress => (omAddress(ra), ra.permissions)
      case _ => throw new IllegalArgumentException()
    }

    OMMemoryRegion(
      name = name,
      description = "",
      addressSets = omRanges,
      permissions = omPerms(permissions),
      registerMap = None // Option[OMRegisterMap]
    )
  }

  private def fmtString(x: ResourceString): Seq[String] = {
    require (legalString(x.value), s"The string '${x.value}' contains chars probably unwise for use in a DTS string")
    Seq("\"" + x.value + "\"")
  }

  private def fmtMap(x: ResourceMap, indent: String, cells: Cells): Seq[String] = {
//    val (nodes, props) = x.value.partition(_ match {
//      case (_, Seq(ResourceMap(_, _))) => true
//      case _ => false
//    })
//
//    def getInt(x: ResourceValue) = x match {
//      case ResourceInt(value) => Some(value.toInt)
//      case _ => None
//    }
//    val selfAddress = x.value.getOrElse("#address-cells", Nil).headOption.flatMap(getInt)
//    val selfSize    = x.value.getOrElse("#size-cells",    Nil).headOption.flatMap(getInt)
//
//    val myCells = Cells(
//      parentAddress = cells.selfAddress,
//      parentSize    = cells.selfSize,
//      selfAddress   = selfAddress.getOrElse(defaultCells.selfAddress),
//      selfSize      = selfSize   .getOrElse(defaultCells.selfSize))
//
//    props.flatMap { case (k, seq) =>
//      require (legalProperty(k), s"The string '${k}' is not a legal DTS property name")
//      seq.headOption match {
//        case None => Seq(indent, k, ";\n")
//        case Some(ResourceString(_)) => {
//          seq.foreach { r => r match {
//            case ResourceString(_) => Unit
//            case _ => require(false, s"The property '${k}' has values of conflicting type: ${seq}")
//          } }
//          Seq(indent, k, " = ", seq.flatMap(z => helper(z, "", myCells)).mkString(", "), ";\n")
//        }
//        case Some(ResourceAlias(_)) => {
//          seq.foreach { r => r match {
//            case ResourceAlias(_) => Unit
//            case _ => require(false, s"The property '${k}' has values of conflicting type: ${seq}")
//          } }
//          Seq(indent, k, " = ", seq.flatMap(z => helper(z, "", myCells)).mkString(", "), ";\n")
//        }
//        case Some(_) => {
//          seq.foreach { r => r match {
//            case ResourceMap(_, _) => require(false, s"The property '${k}' has values of conflicting type: ${seq}")
//            case ResourceString(_) => require(false, s"The property '${k}' has values of conflicting type: ${seq}")
//            case ResourceAlias(_)  => require(false, s"The property '${k}' has values of conflicting type: ${seq}")
//            case _ => Unit
//          } }
//          Seq(indent, k, " = <", seq.flatMap(z => helper(z, "", myCells)).mkString(" "), ">;\n")
//        }
//      }
//    }.toSeq ++
//      nodes.flatMap { case (k, Seq(s: ResourceMap)) =>
//        require (legalNode(k), s"The string '${k}' is not a legal DTS node name")
//        Seq(indent) ++ s.labels.map(_ + ": ").filter(_ => !indent.isEmpty) ++ // labels on root not allowed
//          Seq(k, " {\n") ++ helper(s, indent + "\t", myCells) ++ Seq(indent, "};\n")
//      }
    Seq("")
  }

  private def print(res: ResourceValue, indent: String, cells: Cells): Unit = res match {
    case x: ResourceAddress => println(s"x ResourceAddress: %s".format{fmtAddress(x, cells)})
    case x: ResourceMapping => println(s"x ResourceMapping: %s".format{fmtMapping(x, cells)})
    case x: ResourceInt => println(s"x ResourceInt: %s".format{Seq(x.value.toString)})
    case x: ResourceString => println(s"x ResourceString: %s".format{fmtString(x)})
    case x: ResourceReference => println(s"x ResourceReference: %s".format{Seq("&" + x.value)})
    case x: ResourceAlias => println(s"x ResourceAlias: %s".format{Seq("&" + x.value)})
    case x: ResourceMap => println(s"x ResourceMap: %s".format{fmtMap(x, indent, cells)})
  }

  private def om(name: String, res: ResourceValue): Unit = res match {
    case x: ResourceAddress => omAddress(x)
    case x: ResourceMapping => omMemoryRegion(name, x)
    case x: ResourceInt => println(s"om ResourceInt")
    case x: ResourceString => println(s"om ResourceString")
    case x: ResourceReference => println(s"om ResourceReference")
    case x: ResourceAlias => println(s"om ResourceAlias")
    case x: ResourceMap => println(s"om ResourceMap")
  }

  def getOMInterrupts(resourceBindings: ResourceBindings): Seq[OMInterrupt]= {
    Nil
  }

  def regFilter(name: String): Boolean = name == "reg" || name.take(4) == "reg/"

  def regName(name: String): Option[String] = {
    val keys = name.split("/")
    require (keys.size >= 1 && keys.size <= 2 && keys(0) == "reg", s"Invalid reg name '${name}'")
    if (keys.size == 1) None else Some(keys(1))
  }

  def getOMMemoryRegions(name: String, resourceBindings: ResourceBindings): Seq[OMMemoryRegion]= {
    resourceBindings.map.map {
      case (x: String, seq: Seq[Binding]) if (regFilter(x)) =>
        println(s"ResourceBindings: key = reg/control Binding")
        seq.map {
          case Binding(device: Option[Device], value: ResourceValue) => Some(omMemoryRegion(name, value))
        }
      case _ => None
    }

    val omm = OMMemoryRegion (
      name = name,
      description = "",
      addressSets = Seq[OMAddressSet](),
      permissions = OMPermissions(
        readable = false,
        writeable = false,
        executable = false,
        cacheable = false,
        atomics = false
      ) ,
      registerMap = None // Option[OMRegisterMap]
    )
    Seq(omm)
  }

  def printMR(name: String, seq: Seq[OMMemoryRegion]): Unit = {
    println(s"printMR name = %s".format(name))
    seq.map{
      case mr =>
        println(s"  printMR perms = %s".format(mr.name, mr.permissions))
        mr.addressSets.map{
          case as =>
            println(s"    printMR base = %s mask = %s".format(as.base, as.mask))
        }
    }
  }

}
