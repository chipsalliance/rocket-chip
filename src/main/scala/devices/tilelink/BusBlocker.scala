// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, SimpleDevice}
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc}
import freechips.rocketchip.tilelink.{TLFragmenter, TLRegisterNode, TLBusWrapper, TLNameNode, TLNode}

/** Parameterize a BasicBusBlocker.
  *
  * @param controlAddress    Base address for the device's control registers.
  * @param controlBeatBytes  Datapath width of control port.
  * @param blockedBeatBytes  Datapath width of tilelink channel being blocked. (Used to set internal error device width.)
  * @param deadlock          If true, backpressure all requests while `allow` is false. If false, return error responses while `allow` is false.
  */
case class BasicBusBlockerParams(
  controlAddress:   BigInt,
  controlBeatBytes: Int,
  blockedBeatBytes:  Int,
  deadlock: Boolean = false)
{
  val controlSize = 0x1000
}

/** BasicBusBlocker is an adapter device that allows for software control and monitoring of TL transaction progress.
  *
  * The device uses one register to control whether any accesses are allowed to proceed along their original route,
  * or are instead bypassed to an internal /dev/null device that rejects the transaction in some way.
  * The device uses a second register to report whether any requests are outstanding beyond the blocker,
  * i.e. a transaction is ongoing pending a response from some outward device.
  * The device defaults to allowing transactions to proceed, i.e. `allow` defaults to `TRUE`.
  * Even when `allow` is set to `FALSE`, responses to previously-outstanding transactions are still able to drain.
  */
class BasicBusBlocker(params: BasicBusBlockerParams)(implicit p: Parameters)
    extends TLBusBypassBase(params.blockedBeatBytes, params.deadlock)
{
  val device = new SimpleDevice("basic-bus-blocker", Seq("sifive,basic-bus-blocker0"))

  val controlNode = TLRegisterNode(
    address   = Seq(AddressSet(params.controlAddress, params.controlSize-1)),
    device    = device,
    beatBytes = params.controlBeatBytes)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val allow = RegInit(true.B)
    val pending = RegNext(bar.module.io.pending)

    controlNode.regmap(
      0 -> Seq(RegField  (32, allow,
        RegFieldDesc("allow",
          "Used to enable/disable bus transactions", reset=Some(1)))),
      4 -> Seq(RegField.r(32, pending, RegFieldDesc("pending",
        "Indicates if bus transactions are in-flight", volatile=true)))
    )

    bar.module.io.bypass := !allow
  }
}

object BasicBusBlocker {
  def apply(blockerAddr: BigInt, controlBus: TLBusWrapper, beatBytes: Int, name: String)(implicit p: Parameters): TLNode = {
    apply(Some(blockerAddr), controlBus, beatBytes, name)
  }

  /** This factory optionally creates a BasicBusBlocker with its control port attached to the specified external bus.
    *
    * It returns the datapath node for use in some chain of connections whose traffic is to be blocked.
    */
  def apply(blockerAddrOpt: Option[BigInt], controlBus: TLBusWrapper, beatBytes: Int, name: String)(implicit p: Parameters): TLNode = {
    blockerAddrOpt.map { a =>
      val bus_blocker = LazyModule(new BasicBusBlocker(BasicBusBlockerParams(a, controlBus.beatBytes, beatBytes)))
      controlBus.coupleTo(s"bus_blocker_for_$name") { bus_blocker.controlNode := TLFragmenter(controlBus) := _ }
      bus_blocker.node
    } .getOrElse { TLNameNode(name) }
  }
}
