// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import scala.collection.immutable.SortedMap

object JSON
{
  def apply(res: ResourceValue): String = {
    val root = res match {
      case ResourceMap(value, _) => value.toList match {
        case Seq(("/", Seq(subtree))) => subtree
        case _ => res
      }
      case _ => res
    }
    helper(root)(SortedMap(map(root):_*)).mkString
  }

  private def map(res: ResourceValue, path: String = ""): Seq[(String, String)] = res match {
    case ResourceMap(value, labels) => {
      labels.map(_ -> path) ++
      value.flatMap { case (key, seq) => seq.flatMap(map(_, path + "/" + key)) }
    }
    case _ => Nil
  }

  private def helper(res: ResourceValue)(implicit path: Map[String, String]): Seq[String] = res match {
    case ResourceAddress(address, ResourcePermissions(r, w, x, c, a)) =>
      AddressRange.fromSets(address).map { case AddressRange(base, size) =>
        s"""{"base":${base},"size":${size},"r":${r},"w":${w},"x":${x},"c":${c},"a":${a}}"""}
    case ResourceMapping(address, offset, ResourcePermissions(r, w, x, c, a)) =>
      AddressRange.fromSets(address).map { case AddressRange(base, size) =>
        s"""{"base":${base},"size":${size},"offset":${offset},"r":${r},"w":${w},"x":${x},"c":${c},"a":${a}}"""}
    case ResourceInt(value) => Seq(value.toString)
    case ResourceString(value) => Seq("\"" + value + "\"")
    case ResourceReference(value) => Seq("\"&" + path(value) + "\"")
    case ResourceAlias(value) => Seq("\"&" + path(value) + "\"")
    case ResourceMap(value, _) => {
      Seq(value.map {
        case (key, Seq(v: ResourceMap)) => s""""${key}":${helper(v).mkString}"""
        case (key, seq) => s""""${key}":[${seq.flatMap(helper).mkString(",")}]"""
      }.mkString("{", ",", "}"))
    }
  }
}
