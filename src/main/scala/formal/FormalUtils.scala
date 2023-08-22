// See LICENSE for license details.
package freechips.rocketchip.formal

import chisel3._
import chisel3.util._
import chisel3.experimental.{SourceInfo, SourceLine}
import org.chipsalliance.cde.config.Field

sealed abstract class MonitorDirection(name: String) {
  override def toString: String = name
  def flip: MonitorDirection
}
object MonitorDirection {
  // Also known as master, effectively contains assumes
  object Driver  extends MonitorDirection("Driver") { override def flip: MonitorDirection = Receiver }

  // Also known as slave, effectively contains asserts
  object Receiver extends MonitorDirection("Receiver") { override def flip: MonitorDirection = Driver }

  object Monitor extends MonitorDirection("Monitor") { override def flip: MonitorDirection = Monitor }

  object Cover extends MonitorDirection("Cover") { override def flip: MonitorDirection = Cover }
}

case object TLMonitorStrictMode extends Field[Boolean](true)

case class TestImplType(
  simulation: Boolean = true,
  formal: Boolean = false,
  fpga: Boolean = false
)

// Determine if test should be generated for formal and/or simulation
case object TestplanTestType extends Field[TestImplType](TestImplType())

sealed abstract class PropertyClass(name: String) {
  override def toString: String = name
}

object PropertyClass {
  object Default extends PropertyClass("Default")
  // Properties which should be true by local construction of RTL (not test bench dependent)
  object LocalRTL extends PropertyClass("LocalRTL")
  // Properties that is expected to fail
  object Failure extends PropertyClass("Fail")
  // Properties that may fail because DUT or test is not complete
  object TODO extends PropertyClass("TODO")
  // Properties that may fail because DUT is not complete
  object TODO_RTL extends PropertyClass("TODO_RTL")
  // Disable a cover property and turn it into an assert (for unreachable covers)
  object CoverDisableMonitor extends PropertyClass("CoverDisableMonitor")
}

object Property {
  var prop_name_set = collection.mutable.Set[String]()
  def reset_prop_name: Unit = prop_name_set = collection.mutable.Set[String]()

  def apply(dir: MonitorDirection, cond: Bool, message: String,
            prop_type: PropertyClass=PropertyClass.Default,
            idx: String = "",
            custom_name: String = "")(implicit sourceInfo: SourceInfo): Unit = {
    val line_info = sourceInfo match {
      case SourceLine(filename, line, col) => s"${filename}_L${line}_C${col}_I${idx}".replace('.', '_')
      case _ => ""
    }
    val proposed_src = if (custom_name == "") prop_type.toString + "_" + line_info else custom_name

    val src_wrap = s"@[${proposed_src}]"
    if (dir==MonitorDirection.Monitor) {
      when(!cond) {
        printf(s"assert:${proposed_src}:${prop_type.toString} ${message + "_" + line_info}")
      }
    } else if (dir==MonitorDirection.Receiver) {
      when(!cond) {
        printf(s"assert:${proposed_src}:${prop_type.toString} ${message + "_" + line_info}")
      }
    } else if (dir==MonitorDirection.Driver) {
      when(!cond) {
        printf(s"assume:${proposed_src}:${prop_type.toString} ${message + "_" + line_info}")
      }
    } else if (dir==MonitorDirection.Cover) {
        if (prop_type==PropertyClass.CoverDisableMonitor) {
          when(cond) { //We want to assert that the condition is never true, which is opposite of a normal assertion
            printf(s"assert:${proposed_src}:${prop_type.toString} ${message + "_" + line_info}")
          }
        } else {
            when(cond) {
              printf(s"cover:${proposed_src}:${prop_type.toString} ${message + "_" + line_info}")
            }
        }
    }
  }

  def apply(dir: MonitorDirection, cond: Seq[Seq[Bool]], crosscond: Bool, message: String,
            prop_type: PropertyClass,
            idx: String,
            custom_name: String)(implicit sourceInfo: SourceInfo): Unit = {
    if (cond.isEmpty) {
      Property(dir, crosscond, message, prop_type, idx, custom_name)
    } else {
      cond.head.zipWithIndex.foreach( {case (crossval, cnt) =>
        Property(dir, cond.tail, crossval && crosscond, message, prop_type, idx + "_" + cnt, if (custom_name == "") "" else custom_name + "_" + cnt)
      })
    }
  }
  def apply(dir: MonitorDirection, cond: Seq[Seq[Bool]], crosscond: Bool,
            message: String)(implicit sourceInfo: SourceInfo): Unit = {
    Property(dir, cond, crosscond, message, PropertyClass.Default, "", "")
  }
  def apply(dir: MonitorDirection, cond: Seq[Seq[Bool]], crosscond: Bool, message: String,
            prop_type: PropertyClass)(implicit sourceInfo: SourceInfo): Unit = {
    Property(dir, cond, crosscond, message, prop_type, "", "")
  }
  def apply(cond: Bool)(implicit sourceInfo: SourceInfo): Unit ={
    Property(MonitorDirection.Monitor, cond, "Sanity Property", PropertyClass.LocalRTL, "", "")
  }
}

object SourceGet {
  def get_line_num(implicit sourceInfo: SourceInfo): String = {
    val line_info = sourceInfo match {
      case SourceLine(filename, line, col) => line.toString
      case _ => ""
    }
    line_info
  }
}

object ResetUtils {
  def inactive_output_override[T <: Data](inactive_length: Int)
                                         (sigs: T,
                                          override_assn: (T)=>Unit): Unit = {
    require(inactive_length >= 0)

    if(inactive_length>0) {
      val inactive_counter = RegInit(inactive_length.U)
      when(inactive_counter =/= 0.U) {
        override_assn(sigs)
        inactive_counter := inactive_counter - 1.U
      }
    }

  }
}

object OneHot0Prop {
  def apply(in: Seq[Bool]): Bool = {
    if (in.size==0) { true.B }
    else { apply(Cat(in.reverse)) }
  }
  def apply(in: Vec[Bool]): Bool = {
    if (in.getWidth==0) { true.B }
    else { apply(in.asUInt) }
  }

  def apply(in: Bits): Bool = {
    if(in.getWidth == 0) { true.B }
    else if(in.getWidth == 1) { true.B }
    else {
      val ret_val = Wire(Bool())
      when (in(0) === true.B) { ret_val := ((in>>1).asUInt===0.U) }
      .otherwise { ret_val := apply(in>>1) }
      ret_val
    }
  }
}
object OneHotProp {
  def apply(in: Seq[Bool]): Bool = {
    if (in.size==0) {false.B}
    else { apply(Cat(in.reverse)) }
  }
  def apply(in: Vec[Bool]): Bool = {
    if (in.getWidth==0) {false.B}
    else { apply(in.asUInt) }
  }

  def apply(in: Bits): Bool = {
    if(in.getWidth == 0) { false.B }
    else if(in.getWidth == 1) { in(0) }
    else {
      val ret_val = Wire(Bool())
      when (in(0) === true.B) { ret_val := ((in>>1).asUInt===0.U) }
      .otherwise { ret_val := apply(in>>1) }
      ret_val
    }
  }
}

object OneHotPriorityEncoder {
  def apply(in: Vec[Bool]): Vec[Bool] = {
    VecInit(apply(in.asUInt).asBools)
  }

  def apply(in: Bits): Bits = {
    if(in.getWidth==1) { in }
    else {
      chisel3.util.Mux1H(chisel3.util.UIntToOH(in(0)===true.B),
                         Seq(Cat(apply(in>>1), 0.U(1.W)),
                             1.U(in.getWidth.W)))
    }
  }
}

object IfThen {
  def apply(if_clause: Bool, then_clause: Bool): Bool = {
    !(if_clause) || then_clause
  }
}

object TernaryIf {
  def apply[T <: Data](if_clause: Bool, then_clause: T, else_clause: T): T = {
    val ret_val = Wire(then_clause)
    when(if_clause) {
      ret_val := then_clause
    } .otherwise {
      ret_val := else_clause
    }
    ret_val
  }
}

object Case {
  def apply[T <: Data, R <: Data](case_var: T, sel_ret: Seq[Tuple2[T,R]]): R = {
    val sel_vec = sel_ret.map( (sel_ret_idv: Tuple2[T,R]) => {
      val (select, ret_val) = sel_ret_idv
      TernaryIf(case_var.asUInt===select.asUInt, ret_val.asUInt, 0.U)
    })
    sel_vec.reduce( _ | _).asTypeOf(sel_ret(0)._2)
  }
}

object InSet {
  def apply[T <: Data](data: T, set: Seq[T]): Bool = {
    set.map( _.asUInt === data.asUInt ).reduce(_ || _)
  }
}

object ScalaUtils {
  def repeatfill[T <: Any](size: Int, base_seq: Seq[T]): Seq[T] = {
    if (size==1) {
      Seq(base_seq.head)
    } else {
      base_seq.head +: ScalaUtils.repeatfill(size-1, base_seq.tail :+ base_seq.head)
    }
  }
}
