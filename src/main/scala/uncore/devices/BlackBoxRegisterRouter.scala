// See LICENSE.SiFive for license details.

package uncore.devices

import Chisel._

import config._
import diplomacy._
import regmapper._
import rocketchip.{PeripheryBusConfig}
import uncore.tilelink2._
import util.BlackBoxedRegsWrapper

case class  BlackBoxedRegMapParams(address: BigInt, numRegs: Int, widthBytes: Int)
case object BlackBoxedRegMapKey extends Field[Seq[BlackBoxedRegMapParams]]

trait HasBlackBoxedRegMap extends Module with HasRegMap {
  val params: BlackBoxedRegMapParams
  val w = params.widthBytes*8
  val regs = Module(new BlackBoxedRegsWrapper(params.numRegs, w))
  regmap((regs.io.v.zipWithIndex.map { case(r, i) => i*params.widthBytes -> Seq(RegField.rwReg(w,r)) }):_*)
}

class TLBlackBoxRegisterRouter(val bbrmp: BlackBoxedRegMapParams)(implicit p: Parameters)
  extends TLRegisterRouter(bbrmp.address,  "somedev", Seq("ucbbar,random-interface"), beatBytes = bbrmp.widthBytes)(
  new TLRegBundle(bbrmp, _))(
  new TLRegModule(bbrmp, _, _) with HasBlackBoxedRegMap)
