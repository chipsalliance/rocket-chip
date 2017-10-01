// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util.IrrevocableIO
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import scala.math.{min,max}

// pipe is only used if a queue has depth = 1
class AXI4Buffer(
  aw: BufferParams,
  w:  BufferParams,
  b:  BufferParams,
  ar: BufferParams,
  r:  BufferParams)(implicit p: Parameters) extends LazyModule
{
  def this(aw: BufferParams, br: BufferParams)(implicit p: Parameters) = this(aw, aw, br, aw, br)
  def this(x: BufferParams)(implicit p: Parameters) = this(x, x)
  def this()(implicit p: Parameters) = this(BufferParams.default)

  val node = AXI4AdapterNode(
    masterFn = { p => p },
    slaveFn  = { p => p.copy(minLatency = p.minLatency + min(aw.latency,ar.latency) + min(r.latency,b.latency)) })

  lazy val module = new LazyModuleImp(this) {
    def buffer[T <: Data](config: BufferParams, data: IrrevocableIO[T]): IrrevocableIO[T] = {
      if (config.isDefined) {
        Queue.irrevocable(data, config.depth, pipe=config.pipe, flow=config.flow)
      } else {
        data
      }
    }

    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.aw <> buffer(aw, in .aw)
      out.w  <> buffer(w,  in .w)
      in .b  <> buffer(b,  out.b)
      out.ar <> buffer(ar, in .ar)
      in .r  <> buffer(r,  out.r)
    }
  }
}

object AXI4Buffer
{
  // applied to the AXI4 source node; y.node := AXI4Buffer(x.node)
  def apply()                                  (x: AXI4OutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): AXI4OutwardNode = apply(BufferParams.default)(x)
  def apply(z: BufferParams)                   (x: AXI4OutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): AXI4OutwardNode = apply(z, z)(x)
  def apply(aw: BufferParams, br: BufferParams)(x: AXI4OutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): AXI4OutwardNode = apply(aw, aw, br, aw, br)(x)
  def apply(
    aw: BufferParams,
    w:  BufferParams,
    b:  BufferParams,
    ar: BufferParams,
    r:  BufferParams)(x: AXI4OutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): AXI4OutwardNode = {
    val buffer = LazyModule(new AXI4Buffer(aw, w, b, ar, r))
    buffer.node :=? x
    buffer.node
  }
}
