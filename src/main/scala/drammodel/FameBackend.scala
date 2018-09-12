/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
 California (Regents). All Rights Reserved.  Redistribution and use in
 source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

    * Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer in the documentation and/or other materials
      provided with the distribution.
    * Neither the name of the Regents nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.

 IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
 ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
 ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
 TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 MODIFICATIONS.
*/
/*
package freechips.rocketchip.DRAMModel

import Chisel._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

object FameDecoupledIO
{
  def connect[T <: Bundle](flattened: FameDecoupledIO[Bits], connectTo: FameDecoupledIO[T], tgt_bits_type: Bundle): Unit = {
    val is_flip = (flattened.host_ready.dir == OUTPUT)
    if(is_flip){
      flattened.host_valid := connectTo.host_valid
      connectTo.host_ready := flattened.host_ready
      flattened.target.valid := connectTo.target.valid
      connectTo.target.ready := flattened.target.ready
      flattened.target.bits := connectTo.target.bits.toBits
    } else {
      connectTo.host_valid := flattened.host_valid
      flattened.host_ready := connectTo.host_ready
      connectTo.target.valid := flattened.target.valid
      flattened.target.ready := connectTo.target.ready
      connectTo.target.bits := tgt_bits_type.fromBits(flattened.target.bits)
    }
  }
}

class FameDecoupledIO[T <: Data](data: T) extends Bundle
{
  val host_valid = Bool(OUTPUT)
  val host_ready = Bool(INPUT)
  val target = new DecoupledIO(data)
  override def clone: this.type = { new FameDecoupledIO(data).asInstanceOf[this.type]}
}

class FameQueue[T <: Data] (val entries: Int)(data: => T) extends Module
{
  val io = new Bundle{
    val deq = new FameDecoupledIO(data)
    val enq = new FameDecoupledIO(data).flip()
  }
  
  val target_queue = Module(new Queue(data, entries))
  val tracker = Module(new FameQueueTracker(entries, entries))
  
  target_queue.io.enq.valid := io.enq.host_valid && io.enq.target.valid
  target_queue.io.enq.bits := io.enq.target.bits
  io.enq.target.ready := target_queue.io.enq.ready
  
  io.deq.target.valid := tracker.io.entry_avail && target_queue.io.deq.valid
  io.deq.target.bits := target_queue.io.deq.bits
  target_queue.io.deq.ready := io.deq.host_ready && io.deq.target.ready && tracker.io.entry_avail
  
  tracker.io.tgt_queue_count := target_queue.io.count
  tracker.io.produce := io.enq.host_valid && io.enq.host_ready
  tracker.io.consume := io.deq.host_valid && io.deq.host_ready
  tracker.io.tgt_enq := target_queue.io.enq.valid && target_queue.io.enq.ready
  tracker.io.tgt_deq := io.deq.target.valid && target_queue.io.deq.ready

  io.enq.host_ready := !tracker.io.full && target_queue.io.enq.ready 
  io.deq.host_valid := !tracker.io.empty
  
}

class FameQueueTrackerIO() extends Bundle{
  val tgt_queue_count = UInt(INPUT)
  val produce = Bool(INPUT)
  val consume = Bool(INPUT)
  val tgt_enq = Bool(INPUT)
  val tgt_deq = Bool(INPUT)
  val empty = Bool(OUTPUT)
  val full = Bool(OUTPUT)
  val entry_avail = Bool(OUTPUT)
}

class FameQueueTracker(num_tgt_entries: Int, num_tgt_cycles: Int) extends Module{
  val io = new FameQueueTrackerIO()
  val aregs = Vec.fill(num_tgt_cycles){ Reg(init = UInt(0, width = log2Up(num_tgt_entries))) }
  val tail_pointer = Reg(init = UInt(1, width = log2Up(num_tgt_cycles)))
  
  val next_tail_pointer = UInt()
  tail_pointer := next_tail_pointer
  next_tail_pointer := tail_pointer
  when(io.produce && !io.consume){
    next_tail_pointer := tail_pointer + UInt(1)
  }.elsewhen(!io.produce && io.consume){
    next_tail_pointer := tail_pointer - UInt(1)
  }
  for (i <- 1 until num_tgt_cycles - 1){
    val next_reg_val = UInt()
    aregs(i) := next_reg_val
    next_reg_val := aregs(i)
    when(UInt(i) === tail_pointer){
      when(io.produce && io.tgt_enq && !io.consume){
        next_reg_val := aregs(i - 1) + UInt(1)
      }.elsewhen(io.produce && !io.tgt_enq && !io.consume){
        next_reg_val := aregs(i - 1)
      }
    }.elsewhen(UInt(i) === tail_pointer - UInt(1)){
      when(io.produce && io.tgt_enq && io.consume && io.tgt_deq){
      }.elsewhen(io.produce && io.tgt_enq && io.consume && !io.tgt_deq){
        next_reg_val := aregs(i) + UInt(1)
      }.elsewhen(io.produce && !io.tgt_enq && io.consume && io.tgt_deq){
        next_reg_val := aregs(i) - UInt(1)
      }
    }.otherwise{
      when(io.produce && io.tgt_enq && io.consume && io.tgt_deq){
        next_reg_val := aregs(i + 1) - UInt(1)
      }.elsewhen(io.produce && io.tgt_enq && io.consume && !io.tgt_deq){
        next_reg_val := aregs(i + 1)
      }.elsewhen(io.produce && !io.tgt_enq && io.consume && io.tgt_deq){
        next_reg_val := aregs(i + 1) - UInt(1)
      }.elsewhen(io.produce && !io.tgt_enq && io.consume && !io.tgt_deq){
        next_reg_val := aregs(i + 1)
      }.elsewhen(!io.produce && io.consume && io.tgt_deq){
        next_reg_val := aregs(i + 1) - UInt(1)
      }.elsewhen(!io.produce && io.consume && !io.tgt_deq){
        next_reg_val := aregs(i + 1)
      }
    }
  }
  val next_reg_val0 = UInt()
  aregs(0) := next_reg_val0
  next_reg_val0 := aregs(0)
  when(UInt(0) === tail_pointer){
    when(io.produce && io.tgt_enq && !io.consume){
      next_reg_val0 := io.tgt_queue_count + UInt(1)
    }.elsewhen(io.produce && !io.tgt_enq && io.consume && io.tgt_deq){
    }.elsewhen(io.produce && !io.tgt_enq && io.consume && !io.tgt_deq){
    }.elsewhen(io.produce && !io.tgt_enq && !io.consume){
      next_reg_val0 := io.tgt_queue_count
    }
  }.elsewhen(UInt(0) === tail_pointer - UInt(1)){
    when(io.produce && io.tgt_enq && io.consume && !io.tgt_deq){
      next_reg_val0 := aregs(0) + UInt(1)
    }.elsewhen(io.produce && !io.tgt_enq && io.consume && io.tgt_deq){
      next_reg_val0 := aregs(0) - UInt(1)
    }.elsewhen(io.produce && !io.tgt_enq && io.consume && !io.tgt_deq){
    }
  }.otherwise{
    when(io.produce && io.tgt_enq && io.consume && io.tgt_deq){
      next_reg_val0 := aregs(1) - UInt(1)
    }.elsewhen(io.produce && io.tgt_enq && io.consume && !io.tgt_deq){
      next_reg_val0 := aregs(1)
    }.elsewhen(io.produce && !io.tgt_enq && io.consume && io.tgt_deq){
      next_reg_val0 := aregs(1) - UInt(1)
    }.elsewhen(io.produce && !io.tgt_enq && io.consume && !io.tgt_deq){
      next_reg_val0 := aregs(1)
    }.elsewhen(!io.produce && io.consume && io.tgt_deq){
      next_reg_val0 := aregs(1) - UInt(1)
    }.elsewhen(!io.produce && io.consume && !io.tgt_deq){
      next_reg_val0 := aregs(1)
    }
  }
  val next_reg_val_last = UInt()
  aregs(num_tgt_cycles - 1) := next_reg_val_last
  next_reg_val_last := aregs(num_tgt_cycles - 1)
  when(UInt(num_tgt_cycles - 1) === tail_pointer){
    when(io.produce && io.tgt_enq && io.consume && !io.tgt_deq){
    }.elsewhen(io.produce && io.tgt_enq && !io.consume){
      next_reg_val_last := aregs(num_tgt_cycles - 1 - 1) + UInt(1)
    }.elsewhen(io.produce && !io.tgt_enq && !io.consume){
      next_reg_val_last := aregs(num_tgt_cycles - 1 - 1)
    }
  }.elsewhen(UInt(num_tgt_cycles - 1) === tail_pointer - UInt(1)){
    when(io.produce && io.tgt_enq && io.consume && !io.tgt_deq){
      next_reg_val_last := aregs(num_tgt_cycles - 1) + UInt(1)
    }.elsewhen(io.produce && !io.tgt_enq && io.consume && io.tgt_deq){
      next_reg_val_last := aregs(num_tgt_cycles - 1) - UInt(1)
    }.elsewhen(io.produce && !io.tgt_enq && io.consume && !io.tgt_deq){
    }
  }
  io.full := tail_pointer === UInt(num_tgt_cycles)
  io.empty := tail_pointer === UInt(0)
  io.entry_avail := aregs(0) != UInt(0)
}

class RegIO[T <: Data](data: T) extends Bundle
{
  val bits = data.asOutput
}

class Fame1WrapperIO(num_queues: Int, num_regs: Int, num_debug: Int) extends Bundle {
  var queues:Vec[FameDecoupledIO[Bits]] = null
  if(num_queues > 0) {
    queues = Vec.fill(num_queues){ new FameDecoupledIO(Bits())}
  }
  var regs:Vec[DecoupledIO[Bits]] = null
  if(num_regs > 0) {
    regs = Vec.fill(num_regs){ new DecoupledIO(Bits())}
  }
  var debug:Vec[Bits] = null
  if(num_debug > 0) {
    debug = Vec.fill(num_debug){Bits()}
  }
}

class Fame1Wrapper(f: => Module) extends Module {
  def transform(isTop: Boolean, module: Module, parent: Module): Unit = {
    Fame1Transform.fame1Modules += module
    val isFire = Bool(INPUT)
    isFire.isIo = true
    isFire.setName("is_fire")
    isFire.component = module
    Fame1Transform.fireSignals(module) = isFire
    if(!isTop){
      Predef.assert(Fame1Transform.fireSignals(parent) != null)
      isFire := Fame1Transform.fireSignals(parent)
    }
    module.io.asInstanceOf[Bundle] += isFire
    for(submodule <- module.children){
      transform(false, submodule, module)
    }
  }
  
  val originalModule = Module(f)
  transform(true, originalModule, null)

  //counter number of RegIO and Decoupled IO in original module
  var num_decoupled_io = 0
  var num_reg_io = 0
  var num_debug_io = 0
  for ((name, io) <- originalModule.io.asInstanceOf[Bundle].elements){ 
    io match { 
      case q : DecoupledIO[_] => num_decoupled_io += 1; 
      case r : RegIO[_] => num_reg_io += 1;
      case _ => {
        if (name != "is_fire") {
          num_debug_io += 1
        }
      }
    }
  }

  val io = new Fame1WrapperIO(num_decoupled_io, num_reg_io, num_debug_io)
  
  val RegIOs = new HashMap[String, DecoupledIO[Bits]]()
  val DecoupledIOs  = new HashMap[String, FameDecoupledIO[Bits]]()
  val DebugIOs = new HashMap[String, Data]()

  var decoupled_counter = 0
  var reg_counter = 0
  var debug_counter = 0 
  //populate fame1RegIO and fame1DecoupledIO bundles with the elements from the original RegIO and DecoupleIOs
  for ((name, ioNode) <- originalModule.io.asInstanceOf[Bundle].elements) {
    ioNode match {
      case decoupled : DecoupledIO[_] => {
        val is_flip = (decoupled.ready.dir == OUTPUT)
        val fame1Decoupled      = io.queues(decoupled_counter)
        if (is_flip) {
          fame1Decoupled.flip()
          fame1Decoupled.target.ready := decoupled.ready
          decoupled.valid := fame1Decoupled.target.valid
          val decoupledBitsClone = decoupled.bits.clone()
          decoupled.bits := decoupledBitsClone.fromBits(fame1Decoupled.target.bits)
        } else {
          decoupled.ready := fame1Decoupled.target.ready
          fame1Decoupled.target.bits := decoupled.bits.toBits
          fame1Decoupled.target.valid := decoupled.valid 
        }
        DecoupledIOs(name) = fame1Decoupled
        decoupled_counter += 1
      }
      case reg : RegIO[_] => {
        val is_flip = (reg.bits.flatten(0)._2.dir == INPUT)
        val fame1RegIO = io.regs(reg_counter)
        if (is_flip) {
          fame1RegIO.flip()
          val regBitsClone = reg.bits.clone()
          reg.bits := regBitsClone.fromBits(fame1RegIO.bits)
        } else {
          fame1RegIO.bits := reg.bits.toBits
        }
        RegIOs(name) = fame1RegIO
        reg_counter += 1
      }
      case _ => {
        if (name != "is_fire") {
          Predef.assert(ioNode.isInstanceOf[Bits])
          val elementClone = ioNode.clone
          elementClone.isIo = true
          elementClone.setName(name)
          DebugIOs(name) = elementClone
          elementClone <> ioNode
          if(ioNode.toBits.dir == INPUT){
            io.debug(debug_counter).asInput
            ioNode := io.debug(debug_counter)
            DebugIOs(name) = io.debug(debug_counter)
          } else {
            io.debug(debug_counter).asOutput
            io.debug(debug_counter) := ioNode.toBits
            DebugIOs(name) = io.debug(debug_counter)
          }
          debug_counter += 1
        }
      }
    }
  }
  //generate fire_tgt_clk signal
  var fire_tgt_clk = Bool(true)
  if (io.queues != null){
    for (q <- io.queues)
      fire_tgt_clk = fire_tgt_clk && 
        (if (q.host_valid.dir == OUTPUT) q.host_ready else q.host_valid)
  }
  if (io.regs != null){
    for (r <- io.regs) {
      fire_tgt_clk = fire_tgt_clk && 
        (if (r.valid.dir == OUTPUT) r.ready else r.valid)
    }
  }
  
  //generate host read and host valid signals
  Fame1Transform.fireSignals(originalModule) := fire_tgt_clk
  if (io.queues != null){
    for (q <- io.queues) {
      if (q.host_valid.dir == OUTPUT) 
        q.host_valid := fire_tgt_clk
      else
        q.host_ready := fire_tgt_clk
    }
  }
  if (io.regs != null){
    for (r <- io.regs) {
      if (r.valid.dir == OUTPUT) 
        r.valid := fire_tgt_clk
      else
        r.ready := fire_tgt_clk
    }
  }
}

object Fame1Transform {
  val fame1Modules = new HashSet[Module]
  val fireSignals = new HashMap[Module, Bool]
}

trait Fame1Transform extends Backend {
  private def collectMems(module: Module): ArrayBuffer[(Module, Mem[_])] = {
    val mems = new ArrayBuffer[(Module, Mem[_])]
    //find all the mems in FAME1 modules
    def findMems(module: Module): Unit = {
      if(Fame1Transform.fame1Modules.contains(module)){
        for(mem <- module.nodes.filter(_.isInstanceOf[Mem[_]])){
          mems += ((module, mem.asInstanceOf[Mem[Data]]))
        }
      }
      for(childModule <- module.children){
        findMems(childModule)
      }
    }
    findMems(module)
    return mems
  }
  
  private def appendFireToRegWriteEnables(top: Module) = {
    //find regs that are part of sequential mem read ports
    val mems = collectMems(top)
    val seqMemReadRegs = new HashSet[Reg]
    for((module, mem) <- mems){
      val memSeqReads = mem.seqreads ++ mem.readwrites.map(_.read)
      /*if(mem.seqRead){
        for(memRead <- mem.reads){
          seqMemReadRegs += memRead.addr.inputs(0).asInstanceOf[Reg]
        }
      }*/
      for(memSeqRead <- memSeqReads){
        seqMemReadRegs += memSeqRead.addrReg
      }
    }

    //find all the registers in FAME1 modules
    val regs = new ArrayBuffer[(Module, Reg)]
    def findRegs(module: Module): Unit = {
      if(Fame1Transform.fame1Modules.contains(module)){
        for(reg <- module.nodes.filter(_.isInstanceOf[Reg])){
          if(!seqMemReadRegs.contains(reg.asInstanceOf[Reg])){
            regs += ((module, reg.asInstanceOf[Reg]))
          }
        }
      }
      for(childModule <- module.children){
        findRegs(childModule)
      }
    }
    findRegs(top)
    
    
    for((module, reg) <- regs){
      reg.enable = reg.enable && Fame1Transform.fireSignals(module)
      if(reg.updates.length == 0){
        val regOutput = Bits()
        regOutput.inputs += reg
        val regMux = Bits()
        regMux.inputs += reg.inputs(0)
        reg.inputs(0) = Mux(Fame1Transform.fireSignals(module), regMux, regOutput)
      } else {
        for(i <- 0 until reg.updates.length){
          val wEn = reg.updates(i)._1
          val wData = reg.updates(i)._2
          reg.updates(i) = ((wEn && Fame1Transform.fireSignals(module), wData))
        }
      }
    }
  }
 
  private def appendFireToMemEnables(top: Module) = {
    val mems = collectMems(top)

    for((module, mem) <- mems){
      val memWrites = mem.writes ++ mem.readwrites.map(_.write)
      val memSeqReads = mem.seqreads ++ mem.readwrites.map(_.read)
      for(memWrite <- memWrites){
        if(mem.seqRead){
          if (Driver.backend.isInstanceOf[CppBackend]){
            if(memWrite.inputs(0).asInstanceOf[Data].comp != null && memWrite.inputs(1).asInstanceOf[Data].comp != null){//huge hack for extra MemWrite generated for seqread mems in CPP backed; if both the cond and enable both happen to be directly from registers, this will fail horribly
              memWrite.inputs(1) = memWrite.inputs(1).asInstanceOf[Bool] && Fame1Transform.fireSignals(module)
            } else {
              memWrite.inputs(1) = Bool(false)
            }
          } else {
            memWrite.inputs(1) = memWrite.inputs(1).asInstanceOf[Bool] && Fame1Transform.fireSignals(module)
          }
        } else {
          memWrite.inputs(1) = memWrite.inputs(1).asInstanceOf[Bool] && Fame1Transform.fireSignals(module)
        }
      }
      for(memSeqRead <- memSeqReads){
        Predef.assert(memSeqRead.addrReg.updates.length == 1)
        val oldReadAddr = Bits()
        oldReadAddr.inputs += memSeqRead.addrReg.updates(0)._2
        val oldReadAddrReg = Reg(Bits())
        oldReadAddrReg.comp.component = module
        oldReadAddrReg.comp.asInstanceOf[Reg].enable = if (oldReadAddrReg.comp.asInstanceOf[Reg].isEnable) oldReadAddrReg.comp.asInstanceOf[Reg].enable || Fame1Transform.fireSignals(module) else Fame1Transform.fireSignals(module)
        oldReadAddrReg.comp.asInstanceOf[Reg].isEnable = true
        oldReadAddrReg.comp.asInstanceOf[Reg].updates += ((Fame1Transform.fireSignals(module), oldReadAddr))
        
        val newReadAddr = Mux(Fame1Transform.fireSignals(module), oldReadAddr, oldReadAddrReg)
        
        val oldReadEn = Bool()
        oldReadEn.inputs += memSeqRead.addrReg.updates(0)._1
        val renReg = Reg(init=Bool(false))
        renReg.comp.component = module
        
        renReg.comp.asInstanceOf[Reg].enable = if(renReg.comp.asInstanceOf[Reg].isEnable) renReg.comp.asInstanceOf[Reg].enable || Fame1Transform.fireSignals(module) else Fame1Transform.fireSignals(module)
        renReg.comp.asInstanceOf[Reg].isEnable = true
        renReg.comp.asInstanceOf[Reg].updates += ((Fame1Transform.fireSignals(module), oldReadEn))
        val newRen = Mux(Fame1Transform.fireSignals(module), oldReadEn, renReg)
        
        memSeqRead.addrReg.enable = newRen
        memSeqRead.addrReg.updates.clear
        memSeqRead.addrReg.updates += ((newRen, newReadAddr))
      }
    }
  }
  
  
  preElaborateTransforms += ((top: Module) => collectNodesIntoComp(initializeDFS))
  preElaborateTransforms += ((top: Module) => appendFireToRegWriteEnables(top))
  preElaborateTransforms += ((top: Module) => top.genAllMuxes)
  preElaborateTransforms += ((top: Module) => appendFireToMemEnables(top))
  preElaborateTransforms += ((top: Module) => collectNodesIntoComp(initializeDFS))
  preElaborateTransforms += ((top: Module) => top.genAllMuxes)
}

class Fame1CppBackend extends CppBackend with Fame1Transform
class Fame1VerilogBackend extends VerilogBackend with Fame1Transform
class Fame1FPGABackend extends FPGABackend with Fame1Transform
*/
