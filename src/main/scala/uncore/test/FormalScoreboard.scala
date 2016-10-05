package uncore.test

import Chisel._

class FormalScoreboard [T <: Data] (dtype: T) {
  val nd_value1 = Wire(dtype)
  val nd_value1_reg = Reg(dtype, next = nd_value1, init = nd_value1)
  nd_value1 := nd_value1_reg
  
  val nd_value2_prop = Wire(dtype)
  val nd_value2_reg = Reg(dtype, next = nd_value2_prop, init = nd_value2_prop)
  nd_value2_prop := nd_value2_reg
  
  val nd_value2 = Wire(dtype)

  when (nd_value1.asUInt === nd_value2_prop.asUInt) {
    nd_value2 := ~nd_value1.asUInt
  } .otherwise {
    nd_value2 := nd_value2_prop
  }

  def generator (valid: Bool = Bool(false), nd_value1:T=nd_value1, nd_value2:T=nd_value2) : T = {
    val nd_select_reg_activate = RegInit(Bool(false))
    val nd_select_reg_deactivate = RegInit(Bool(false))

    val random_val = NoiseMaker(true, wide=1, Bool(true)).toBool

    val out_val = Wire(dtype)

    when (valid && !nd_select_reg_activate && random_val) {
      nd_select_reg_activate := Bool(true)
      out_val := nd_value2
    } .elsewhen (valid && nd_select_reg_activate && !nd_select_reg_deactivate) {
      nd_select_reg_deactivate := Bool(true)
      out_val := nd_value2
    } otherwise {
      out_val := nd_value1
    }
    out_val
  }

  def checker (data_in : T, valid: Bool = Bool(false), nd_value1:T=nd_value1, nd_value2:T=nd_value2) {
    val nd_selected = RegInit(Bool(false))
    val nd_selected2 = RegInit(Bool(false))
    
    when (valid && !nd_selected && data_in.asUInt===nd_value2.asUInt) {
      nd_selected := Bool(true)
    } .elsewhen (valid && nd_selected && !nd_selected2 && data_in.asUInt===nd_value2.asUInt) {
      nd_selected2 := Bool(true)
    }
    
    // valid && (!nd_selected && !nd_selected2) -> 
    //    (data_in.asUInt === nd_value2.asUInt || data_in.asUInt===nd_value1.asUint)
    assert( !(valid && (!nd_selected && !nd_selected2)) || 
            (data_in.asUInt === nd_value1.asUInt ||
             data_in.asUInt === nd_value2.asUInt))

    // If already seen the first value, next value must be active
    // valid && (nd_selected && !nd_selected2) -> data_in.asUInt === nd_value2.asUInt
    assert( !(valid && (nd_selected && !nd_selected2)) || 
            data_in.asUInt === nd_value2.asUInt)
            
    // If already seen both values, then next values must be inactive
    // valid && (nd_selected && nd_selected2) -> data_in.asUInt === nd_value1.asUInt
    assert( !(valid && (nd_selected && nd_selected2)) || 
            data_in.asUInt === nd_value1.asUInt)
  }
}