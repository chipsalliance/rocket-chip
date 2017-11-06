// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel._
import chisel3.shim.CloneModule

class MixedTestNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data](
  node: NodeHandle [DI, UI, EI, BI, DO, UO, EO, BO], clone: CloneLazyModule)(
  implicit valName: ValName)
  extends MixedCustomNode(node.inner, node.outer)(
    numPI = node.inward .uiParams.size to node.inward .uiParams.size,
    numPO = node.outward.doParams.size to node.outward.doParams.size)
{
  // The devices connected to this test node must recreate these parameters:
  def sourceParams: Seq[DI] = node.inward .diParams
  def sinkParams:   Seq[UO] = node.outward.uoParams

  def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    require (oStars <= 1, s"${name} (a test node) appears right of a :=* ${oStars} times; at most once is allowed${lazyModule.line}")
    require (iStars <= 1, s"${name} (a test node) appears left of a :*= ${iStars} times; at most once is allowed${lazyModule.line}")
    (node.inward.uiParams.size - iKnown, node.outward.doParams.size - oKnown)
  }

  def mapParamsU(n: Int, p: Seq[UO]): Seq[UI] = node.inward .uiParams
  def mapParamsD(n: Int, p: Seq[DI]): Seq[DO] = node.outward.doParams

  override protected[diplomacy] def instantiate() = {
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

final class CloneLazyModule private (val base: LazyModule)
{
  def clone[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data](node: NodeHandle[DI, UI, EI, BI, DO, UO, EO, BO])(implicit valName: ValName) =
    new MixedTestNode(node, this)

  lazy val io = CloneModule(base.module)
}

object CloneLazyModule
{
  def apply(base: LazyModule) = new CloneLazyModule(base)
}
