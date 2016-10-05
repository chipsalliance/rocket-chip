package uncore.test

import Chisel._

object LFSR64
{ 
  private var counter = 0 
  private def next: Int = {
    counter += 1
    counter
  }
  
  def apply(increment: Bool = Bool(true), seed: Int = next): UInt =
  { 
    val wide = 64
    val lfsr = RegInit(UInt((seed * 0xDEADBEEFCAFEBAB1L) >>> 1, width = wide))
    val xor = lfsr(0) ^ lfsr(1) ^ lfsr(3) ^ lfsr(4)
    when (increment) { lfsr := Cat(xor, lfsr(wide-1,1)) }
    lfsr
  }
}

trait HasNoiseMakerIO
{
  val io = new Bundle {
    val inc = Bool(INPUT)
    val random = UInt(OUTPUT)
  }
}

class LFSRNoiseMaker(wide: Int) extends Module with HasNoiseMakerIO
{
  val lfsrs = Seq.fill((wide+63)/64) { LFSR64(io.inc) }
  io.random := Cat(lfsrs)(wide-1,0)
}

class FormalNoiseMaker(wide: Int,
                       num_abstractions:Int,
                       abstraction_values:Vec[UInt])
    extends Module with HasNoiseMakerIO {

  require (abstraction_values.length <= num_abstractions)
  if (num_abstractions > 1) {
    val nd_wires = Vec.fill(num_abstractions) { Wire(UInt(wide)) }
    val nd_values = Vec.tabulate(num_abstractions) (
        (i:Int) =>
        if (i < abstraction_values.length) 
          Reg(UInt(wide), next=nd_wires(i), init=abstraction_values(i))
          else Reg(UInt(wide), next=nd_wires(i), init=nd_wires(i))
        )
    nd_wires := nd_values

    val nd_select = Wire(Bits(width=log2Ceil(num_abstractions)))
    val nd_select_limited = Wire(Bits(width=log2Ceil(num_abstractions)))
    
    // assume nd_select < num_abstractions
    when (nd_select >= UInt(num_abstractions)) {
      nd_select_limited := UInt(0)
    } .otherwise {
      nd_select_limited := nd_select
    }
    
    val reg_select = RegEnable(nd_select, io.inc)
    io.random := nd_values(reg_select)
  } else if (num_abstractions == 1) {
    if (abstraction_values.length > 0) {
      io.random := abstraction_values(0)
    } else {
      val nd_wires = Wire(UInt(wide))
      val nd_values = Reg(UInt(wide), next=nd_wires, init=nd_wires)
      nd_wires := nd_values
      io.random := nd_values
    }
  } else {
    val nd_value = Wire(UInt(width=wide))
    io.random := RegEnable(nd_value, io.inc) 
  }
}

object NoiseMaker {
  def apply (wide:Int): UInt = {
    val nm = Module(new LFSRNoiseMaker(wide))
    nm.io.inc := Bool(true)
    nm.io.random
  }

  def apply (wide:Int, increment: Bool): UInt = {
    val nm = Module(new LFSRNoiseMaker(wide))
    nm.io.inc := increment
    nm.io.random
  }

  def apply (formalmode: Boolean, wide:Int, increment: Bool): UInt = {
    if (formalmode) {
      val rnd_mod = Module(new FormalNoiseMaker(wide=wide,
                                                num_abstractions=0,
                                                abstraction_values=Vec(0, UInt(0))))
      rnd_mod.io.inc := increment
      rnd_mod.io.random
    } else {
      val nm = Module(new LFSRNoiseMaker(wide))
      nm.io.inc := increment
      nm.io.random
    }
  }

  def apply (formalmode : Boolean,
             wide: Int,
             increment: Bool,
             num_abstractions : Int
            ) : UInt = {
    if (formalmode) {
      val rnd_mod = Module(new FormalNoiseMaker(wide,
                                                num_abstractions,
                                                Vec(0, UInt(0))))
      rnd_mod.io.inc := increment
      rnd_mod.io.random
    } else {
      val nm = Module(new LFSRNoiseMaker(wide))
      nm.io.inc := increment
      nm.io.random
    }
  }

  def apply (formalmode : Boolean,
             wide:Int,
             increment: Bool,
             num_abstractions : Int,
             abstraction_values:Vec[UInt]
            ) : UInt = {
    if (formalmode) {
      val rnd_mod = Module(new FormalNoiseMaker(wide, num_abstractions, abstraction_values))
      rnd_mod.io.inc := increment
      rnd_mod.io.random
    } else {
      val nm = Module(new LFSRNoiseMaker(wide))
      nm.io.inc := increment
      nm.io.random
    }
  }
}
