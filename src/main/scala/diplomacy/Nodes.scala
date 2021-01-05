// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel._

/** A node intended to replace a portion of the diplomatic graph in order to test functionality of a copy (cloned) [[LazyModule]]
  *
  * @param node  the node to copy
  * @param clone the copy of the LazyModule containing [[node]]
  */
class MixedTestNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data] (
  node: NodeHandle [DI, UI, EI, BI, DO, UO, EO, BO], clone: CloneLazyModule)(
  implicit valName: ValName)
  extends MixedCustomNode(node.inner, node.outer)
{
  override def description = "test"
  def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    def resolveStarInfo: String =
      s"""$context
         |$bindingInfo
         |number of known := bindings to inward nodes: $iKnown
         |number of known := bindings to outward nodes: $oKnown
         |number of binding queries from inward nodes: $iStars
         |number of binding queries from outward nodes: $oStars
         |downstream inward parameters: ${node.inward.getDiParams}
         |upstream inward parameters: ${node.inward.getUiParams}
         |upstream outward parameters: ${node.outward.getUoParams}
         |downstream outward parameters: ${node.outward.getDoParams}
         |node.inward.uiParams.size
         |""".stripMargin

    require(oStars <= 1,
      s"""Diplomacy has detected a problem with your graph:
         |The following node appears right of a :=* $oStars times; at most once is allowed.
         |$resolveStarInfo
         |""".stripMargin)
    require(iStars <= 1,
      s"""Diplomacy has detected a problem with your graph:
         |The following node appears left of a :*= $iStars times; at most once is allowed.
         |$resolveStarInfo
         |""".stripMargin)
    require(node.inward .getUiParams.size == iKnown || iStars == 1,
      s"""Diplomacy has detected a problem with your graph:
         |The following node has only $iKnown inputs, which should be ${node.inward.getUiParams.size}, or connect this node on the left-hand side of :*=
         |$resolveStarInfo
         |""".stripMargin)
    require(node.outward.getDoParams.size == oKnown || oStars == 1,
      s"""Diplomacy has detected a problem with your graph:
         |The following node has only $oKnown outputs, which should be ${node.outward.getDoParams.size}, or connect this node on the right-hand side of :=*
         |$resolveStarInfo
         |""".stripMargin)
    (node.inward.getUiParams.size - iKnown, node.outward.getDoParams.size - oKnown)
  }

  def mapParamsU(n: Int, p: Seq[UO]): Seq[UI] = node.inward .getUiParams
  def mapParamsD(n: Int, p: Seq[DI]): Seq[DO] = node.outward.getDoParams

  override def instantiate(): Seq[Dangle] = {
    val dangles = super.instantiate()
    val orig_module = clone.base.module
    val clone_auto = clone.io("auto").asInstanceOf[AutoBundle]

    danglesOut.zipWithIndex.foreach { case (d, i) =>
      val orig = orig_module.dangles.find(_.source == HalfEdge(node.outward.serial, i))
      require (orig.isDefined, s"Cloned node ${node.outward.name} must be connected externally out ${orig_module.name}")
      val io_name = orig_module.auto.elements.find(_._2 eq orig.get.data).get._1
      d.data <> clone_auto.elements(io_name)
    }
    danglesIn.zipWithIndex.foreach { case (d, i) =>
      val orig = orig_module.dangles.find(_.sink == HalfEdge(node.inward.serial, i))
      require (orig.isDefined, s"Cloned node ${node.inward.name} must be connected externally in ${orig_module.name}")
      val io_name = orig_module.auto.elements.find(_._2 eq orig.get.data).get._1
      clone_auto.elements(io_name) <> d.data
    }

    dangles
  }
}
