// See LICENSE.Berkeley for license details.

package freechips.rocketchip.tilelink

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

object TLBundleBridgeImp extends BundleBridgeImp[TLBundle]

case class TLToBundleBridgeNode(managerParams: TLManagerPortParameters)(implicit valName: ValName)
  extends MixedAdapterNode(TLImp, TLBundleBridgeImp)(
    dFn = { masterParams =>
      BundleBridgeParams(() => TLBundle(TLBundleParameters(masterParams, managerParams)))
    },
    uFn = { mp => managerParams }
  )

object TLToBundleBridgeNode {
  def apply(managerParams: TLManagerParameters, beatBytes: Int)(implicit  valName: ValName): TLToBundleBridgeNode =
    new TLToBundleBridgeNode(TLManagerPortParameters(Seq(managerParams), beatBytes))
}

class TLToBundleBridge(managerParams: TLManagerPortParameters)(implicit p: Parameters) extends LazyModule {
  val node = TLToBundleBridgeNode(managerParams)
  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, _), (out, _)) =>
      out <> in
    }
  }
}

object TLToBundleBridge {
  def apply(managerParams: TLManagerPortParameters)(implicit p: Parameters): TLToBundleBridgeNode = {
    val converter = LazyModule(new TLToBundleBridge(managerParams))
    converter.node
  }
  def apply(managerParams: TLManagerParameters, beatBytes: Int)(implicit p: Parameters): TLToBundleBridgeNode = {
    apply(TLManagerPortParameters(Seq(managerParams), beatBytes))
  }
}

case class BundleBridgeToTLNode(clientParams: TLClientPortParameters)(implicit valName: ValName)
  extends MixedAdapterNode(TLBundleBridgeImp, TLImp)(
    dFn = { mp =>
      clientParams
    },
    uFn = { managerParams =>
      BundleBridgeParams(() => TLBundle(TLBundleParameters(clientParams, managerParams)))
    }
  )

object BundleBridgeToTLNode {
  def apply(clientParams: TLClientParameters, beatBytes: Int)(implicit valName: ValName): BundleBridgeToTLNode = {
    BundleBridgeToTLNode(TLClientPortParameters(Seq(clientParams), beatBytes))
  }
}

class BundleBridgeToTL(clientParams: TLClientPortParameters)(implicit p: Parameters) extends LazyModule {
  val node = BundleBridgeToTLNode(clientParams)
  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, _), (out, _)) =>
      out <> in
    }
  }
}

object BundleBridgeToTL {
  def apply(clientParams: TLClientPortParameters)(implicit p: Parameters): BundleBridgeToTLNode = {
    val converter = LazyModule(new BundleBridgeToTL(clientParams))
    converter.node
  }
  def apply(clientParams: TLClientParameters, beatBytes: Int)(implicit p: Parameters): BundleBridgeToTLNode = {
    apply(TLClientPortParameters(Seq(clientParams), beatBytes))
  }
}

object TLIO {
  def client(clientParams: TLClientPortParameters)(implicit valName: ValName, p: Parameters) = {
    val io = BundleBridgeNullSource[TLBundle]()(valName)
    val cconverter = BundleBridgeToTL(clientParams)
    cconverter := io

    (
      InModuleBody { io.makeIO() },
      cconverter
    )
  }

  def manager(managerParams: TLManagerPortParameters)(implicit valName: ValName, p: Parameters) = {
    val io = BundleBridgeSink[TLBundle]()
    val mconverter = TLToBundleBridge(managerParams)
    io := mconverter

    (
      InModuleBody { io.makeIO() },
      mconverter
    )
  }
}
