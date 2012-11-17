package rocket

import Chisel._
import Node._
import Constants._
import hwacha._
import Util._

class ioRocket(implicit conf: RocketConfiguration) extends Bundle
{
  val host    = new ioHTIF(conf.ntiles)
  val imem    = new IOCPUFrontend()(conf.icache)
  val vimem   = new IOCPUFrontend()(conf.icache)
  val dmem    = new ioHellaCache()(conf.dcache)
}

class Core(implicit conf: RocketConfiguration) extends Component
{
  val io    = new ioRocket
   
  val ctrl  = new Control
  val dpath = new Datapath

  ctrl.io.dpath <> dpath.io.ctrl
  dpath.io.host <> io.host

  ctrl.io.imem <> io.imem
  dpath.io.imem <> io.imem

  val dmemArb = new HellaCacheArbiter(2 + conf.vec)
  dmemArb.io.mem <> io.dmem
  val dmem = dmemArb.io.requestor
  dmem(1) <> ctrl.io.dmem
  dmem(1) <> dpath.io.dmem

  val ptw = collection.mutable.ArrayBuffer(io.imem.ptw, io.dmem.ptw)

  val fpu: FPU = if (conf.fpu) {
    val fpu = new FPU(4,6)
    dpath.io.fpu <> fpu.io.dpath
    ctrl.io.fpu <> fpu.io.ctrl
    fpu
  } else null

  if (conf.vec) {
    val vu = new vu()

    val vdtlb = new TLB(8)
    ptw += vdtlb.io.ptw
    vdtlb.io <> vu.io.vtlb

    val pftlb = new TLB(2)
    pftlb.io <> vu.io.vpftlb
    ptw += pftlb.io.ptw

    dpath.io.vec_ctrl <> ctrl.io.vec_dpath

    // hooking up vector I$
    ptw += io.vimem.ptw
    io.vimem.req.bits.pc := vu.io.imem_req.bits
    io.vimem.req.valid := vu.io.imem_req.valid
    io.vimem.invalidate := ctrl.io.imem.invalidate
    vu.io.imem_resp.valid := io.vimem.resp.valid
    vu.io.imem_resp.bits.pc := io.vimem.resp.bits.pc
    vu.io.imem_resp.bits.data := io.vimem.resp.bits.data
    vu.io.imem_resp.bits.xcpt_ma := io.vimem.resp.bits.xcpt_ma
    vu.io.imem_resp.bits.xcpt_if := io.vimem.resp.bits.xcpt_if
    io.vimem.resp.ready := vu.io.imem_resp.ready
    io.vimem.req.bits.mispredict := Bool(false)
    io.vimem.req.bits.taken := Bool(false)

    ctrl.io.vec_iface.vcmdq <> vu.io.vcmdq
    ctrl.io.vec_iface.vximm1q <> vu.io.vximm1q
    ctrl.io.vec_iface.vximm2q <> vu.io.vximm2q
    ctrl.io.vec_iface.vcntq <> vu.io.vcntq

    dpath.io.vec_iface.vcmdq <> vu.io.vcmdq
    dpath.io.vec_iface.vximm1q <> vu.io.vximm1q
    dpath.io.vec_iface.vximm2q <> vu.io.vximm2q
    dpath.io.vec_iface.vcntq <> vu.io.vcntq

    ctrl.io.vec_iface.vpfcmdq <> vu.io.vpfcmdq
    ctrl.io.vec_iface.vpfximm1q <> vu.io.vpfximm1q
    ctrl.io.vec_iface.vpfximm2q <> vu.io.vpfximm2q
    ctrl.io.vec_iface.vpfcntq <> vu.io.vpfcntq

    dpath.io.vec_iface.vpfcmdq <> vu.io.vpfcmdq
    dpath.io.vec_iface.vpfximm1q <> vu.io.vpfximm1q
    dpath.io.vec_iface.vpfximm2q <> vu.io.vpfximm2q
    dpath.io.vec_iface.vpfcntq <> vu.io.vpfcntq

    // user level vector command queue ready signals
    ctrl.io.vec_iface.vcmdq_user_ready := vu.io.vcmdq_user_ready
    ctrl.io.vec_iface.vximm1q_user_ready := vu.io.vximm1q_user_ready
    ctrl.io.vec_iface.vximm2q_user_ready := vu.io.vximm2q_user_ready

    // fences
    ctrl.io.vec_iface.vfence_ready := vu.io.vfence_ready

    // irqs
    ctrl.io.vec_iface.irq := vu.io.irq
    ctrl.io.vec_iface.irq_cause := vu.io.irq_cause
    dpath.io.vec_iface.irq_aux := vu.io.irq_aux

    // exceptions
    vu.io.xcpt.exception := ctrl.io.vec_iface.exception
    vu.io.xcpt.evac := ctrl.io.vec_iface.evac
    vu.io.xcpt.evac_addr := dpath.io.vec_iface.evac_addr.toUFix
    vu.io.xcpt.kill := ctrl.io.vec_iface.kill
    vu.io.xcpt.hold := ctrl.io.vec_iface.hold

    // hooking up vector memory interface
    dmem(2).req.bits.data := Reg(StoreGen(vu.io.dmem_req.bits.typ, Bits(0), vu.io.dmem_req.bits.data).data)
    dmem(2).req <> vu.io.dmem_req
    dmem(2).resp <> vu.io.dmem_resp

    // DON'T share vector integer multiplier with rocket
    vu.io.cp_imul_req.valid := Bool(false)

    // share sfma and dfma pipelines with rocket
    require(conf.fpu)
    fpu.io.sfma <> vu.io.cp_sfma
    fpu.io.dfma <> vu.io.cp_dfma
  } else if (conf.fpu) {
    fpu.io.sfma.valid := Bool(false)
    fpu.io.dfma.valid := Bool(false)
  }

  val thePTW = new PTW(ptw.length)
  ptw zip thePTW.io.requestor map { case (a, b) => a <> b }
  thePTW.io.dpath <> dpath.io.ptw
  dmem(0) <> thePTW.io.mem
}
