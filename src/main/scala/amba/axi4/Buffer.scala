// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import chisel3._
import chisel3.util.{Queue, IrrevocableIO}

import org.chipsalliance.cde.config.Parameters

import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

import freechips.rocketchip.diplomacy.BufferParams

import scala.math.min

/**
  * Add buffers to AXI4 channels
  *
  * Pipe is only used if a queue has depth = 1
  */
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
    managerFn = { p => p },
    subordinateFn  = { p => p.copy(minLatency = p.minLatency + min(aw.latency,ar.latency) + min(r.latency,b.latency)) })

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
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
  def apply()                                  (implicit p: Parameters): AXI4Node = apply(BufferParams.default)
  def apply(z: BufferParams)                   (implicit p: Parameters): AXI4Node = apply(z, z)
  def apply(aw: BufferParams, br: BufferParams)(implicit p: Parameters): AXI4Node = apply(aw, aw, br, aw, br)
  def apply(
    aw: BufferParams,
    w:  BufferParams,
    b:  BufferParams,
    ar: BufferParams,
    r:  BufferParams)(implicit p: Parameters): AXI4Node =
  {
    val axi4buf = LazyModule(new AXI4Buffer(aw, w, b, ar, r))
    axi4buf.node
  }
}
