// See LICENSE.SiFive for license details.

package freechips.rocketchip.util.property

import Chisel._
import chisel3.internal.sourceinfo.{SourceInfo, SourceLine}
import chisel3.util.{ReadyValidIO}
import chisel3.core.{VecInit}

/*def assert(cond: Bool, message: String): Unit = {
  Property(monitorDir,
      cond,
      message,
      PropertyClass.Default,
      desc_text)
}

def assume(cond: Bool, message: String): Unit = {
  Property(monitorDir.flip,
      cond,
      message,
      PropertyClass.Default,
      desc_text)
}*/

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

trait BasePropertyParameters {
  val pType: MonitorDirection
  val cond: Bool
  val label: String
  val message: String
}

case class DriverPropertyParameters(
    cond: Bool,
    label: String = "",
    message: String = "") extends BasePropertyParameters {
  val pType = MonitorDirection.Driver
}

case class ReceiverPropertyParameters(
    cond: Bool,
    label: String = "",
    message: String = "") extends BasePropertyParameters {
  val pType = MonitorDirection.Receiver
}

case class MonitorPropertyParameters(
    cond: Bool,
    label: String = "",
    message: String = "") extends BasePropertyParameters {
  val pType = MonitorDirection.Monitor
}

case class CoverPropertyParameters(
    cond: Bool,
    label: String = "",
    message: String = "") extends BasePropertyParameters {
  val pType = MonitorDirection.Cover
}

abstract class BasePropertyLibrary {
  def generateProperty(prop_param: BasePropertyParameters)(implicit sourceInfo: SourceInfo)
}

class DefaultPropertyLibrary extends BasePropertyLibrary {
  def generateProperty(prop_param: BasePropertyParameters)(implicit sourceInfo: SourceInfo) {
    // default is to do nothing
    Unit
  }
}

abstract class BaseProperty {
  def generateProperties(): Seq[BasePropertyParameters]
}

case class CoverBoolean(cond: Bool, labels: Seq[String]) {
}

sealed abstract class PropertyClass(name: String) {
  override def toString: String = name
}

object PropertyClass {
  object Default extends PropertyClass("Default")
  // Properties which should be true by local construction of RTL (not test bench dependent)
  object LocalRTL extends PropertyClass("LocalRTL")
  // Properties that is expected to fail
  object CoverDisableMonitor extends PropertyClass("CoverDisableMonitor")
}

abstract class Property extends BaseProperty{}

object Property extends BaseProperty{
  private var propLib: BasePropertyLibrary = new DefaultPropertyLibrary
  def setPropLib(lib: BasePropertyLibrary): Unit = this.synchronized {
    propLib = lib
  }
  var prop_name_set = collection.mutable.Set[String]()
  def reset_prop_name: Unit = prop_name_set = collection.mutable.Set[String]()

  def apply(dir: MonitorDirection, cond: Bool, message: String,
            prop_type: PropertyClass=PropertyClass.Default,
            idx: String = "")(implicit sourceInfo: SourceInfo): Unit = {
    val line_info = sourceInfo match {
      case SourceLine(filename, line, col) => s"${filename}_L${line}_C${col}_I${idx}".replace('.', '_')
      case _ => ""
    }
    val proposed_src = prop_type.toString + "_" + line_info

//    var max_rand = 15
//    val random_src = scala.util.Random
//    while (!prop_name_set.add(proposed_src)) {
//      proposed_src = src + "_" + "%x".format(random_src.nextInt(max_rand))
//      max_rand = max_rand * 2
//    }
    val src_wrap = s"@[${proposed_src}]"
    if (dir==MonitorDirection.Monitor) {
//      assert(cond, s"Assert: ${prop_type.toString} ${message} ${src_wrap}")
      propLib.generateProperty(MonitorPropertyParameters(!cond, prop_type.toString, message))
      when(!cond) {
        printf(s"assert:${proposed_src}:${prop_type.toString} ${message}")
      }
    } else if (dir==MonitorDirection.Receiver) {
      propLib.generateProperty(ReceiverPropertyParameters(!cond, prop_type.toString, message))
//      assert(cond, s"Assert: ${prop_type.toString} ${message} ${src_wrap}")
      when(!cond) {
        printf(s"assert:${proposed_src}:${prop_type.toString} ${message}")
      }
    } else if (dir==MonitorDirection.Driver) {
      propLib.generateProperty(ReceiverPropertyParameters(!cond, prop_type.toString, message))
//      assert(cond, s"Assume: ${prop_type.toString} ${message} ${src_wrap}")
      when(!cond) {
        printf(s"assume:${proposed_src}:${prop_type.toString} ${message}")
      }
    } else if (dir==MonitorDirection.Cover) {
        if (prop_type==PropertyClass.CoverDisableMonitor) {
          propLib.generateProperty(MonitorPropertyParameters(cond, prop_type.toString, message))
//          assert(!cond, s"Assert: ${prop_type.toString} ${message} ${src_wrap}")
          when(cond) { //We want to assert that the condition is never true, which is opposite of a normal assertion
            printf(s"assert:${proposed_src}:${prop_type.toString} ${message}")
          }
        } else {
          propLib.generateProperty(CoverPropertyParameters(cond, prop_type.toString, message))
//        chisel3.core.withReset(false.B) {
//          assert(!cond, s"Cover: ${prop_type.toString} ${message} ${src_wrap}")
            when(cond) {
              printf(s"cover:${proposed_src}:${prop_type.toString} ${message}")
            }
//        }
        }
    }
  }

  def apply(cond: Bool)(implicit sourceInfo: SourceInfo): Unit ={
    Property(MonitorDirection.Monitor, cond, "Sanity Property", PropertyClass.LocalRTL, "")
  }
}

// CrossProperty.generateProperties() will generate Boolean crosses for the cond sequence
//  E.g.
//   Cond = [ [A1, A2, A3],
//            [B2],
//            [C1, C2] ]
//  It will generate the following properties
//   [ A1 && B2 && C1,
//     A1 && B2 && C2,
//     A2 && B2 && C1,
//     A2 && B2 && C2,
//     A3 && B2 && C1,
//     A3 && B2 && C2 ]
//  Each of the boolean expression (A1, A2, C1, etc.) have a label associated with it
//    User can exclude a particular cross from being generated by adding it to the exclude list
//  e.g.
//   exclude = [ ["A1_label", "C2_Label"],
//               ["A3_label", "B2_label"] ]
//  will exclude all crosses with those labels, so the new cross list will be 
//   [ A1 && B2 && C1,
//     A2 && B2 && C1,
//     A2 && B2 && C2 ]

//  Each boolean expression can be associated with more than one label

class CrossProperty(cond: Seq[Seq[CoverBoolean]], exclude: Seq[Seq[String]], message: String) extends Property {
  def listProperties(c1: CoverBoolean, c2: Seq[CoverBoolean]): Seq[CoverBoolean] = {
    if (c2.isEmpty) {
      Seq(c1)
    } else {
      c2.map( (c: CoverBoolean) => {
        new CoverBoolean(c1.cond && c.cond, c1.labels ++ c.labels)
      })
    }
  }

  def crossProperties(cond: Seq[Seq[CoverBoolean]]): Seq[CoverBoolean] = {
    if (cond.isEmpty) {
      Seq()
    } else {
      cond.head.map( (c1: CoverBoolean) => {
        listProperties(c1, crossProperties(cond.tail))
      }).reduce(_ ++ _)
    }
  }
  def inSequence(search: Seq[String], find: Seq[String]): Boolean = {
    if (find.isEmpty) {
      true
    } else {
      find.map( (s:String) => {
        search.contains(s)
      }).reduce( _ && _ )
    }
  }
  def SeqsinSequence(search: Seq[String], find: Seq[Seq[String]]): Boolean = {
    if (find.isEmpty) {
      false
    } else {
      find.map( (s: Seq[String]) => {
        inSequence(search, s)
      }).reduce (_ || _)
    }
  }

  def generateProperties(): Seq[CoverPropertyParameters] = {
    crossProperties(cond).filter(c => !SeqsinSequence(c.labels, exclude)).map( (c: CoverBoolean) => {
      new CoverPropertyParameters(
        cond = c.cond,
        label = c.labels.reduce( (s1: String, s2: String) => {s1 + "_" + s2} ),
        message = message + " " + c.labels.map("<" + _ + ">").reduce ( (s1: String, s2: String) => { s1 + " X " + s2 }))
    })
  }

}

object driver {
  private var propLib: BasePropertyLibrary = new DefaultPropertyLibrary
  def setPropLib(lib: BasePropertyLibrary): Unit = this.synchronized {
    propLib = lib
  }
  def apply(cond: Bool)(implicit sourceInfo: SourceInfo): Unit = {
    propLib.generateProperty(DriverPropertyParameters(cond))
  }
  def apply(cond: Bool, label: String)(implicit sourceInfo: SourceInfo): Unit = {
    propLib.generateProperty(DriverPropertyParameters(cond, label))
  }
  def apply(cond: Bool, label: String, message: String)(implicit sourceInfo: SourceInfo): Unit = {
    propLib.generateProperty(DriverPropertyParameters(cond, label, message))
  }
  def apply(prop: BaseProperty)(implicit sourceInfo: SourceInfo): Unit = {
    prop.generateProperties().foreach( (pp: BasePropertyParameters) => {
      if (pp.pType == MonitorDirection.Driver) {
        propLib.generateProperty(DriverPropertyParameters(pp.cond, pp.label, pp.message))
      }
    })
  }
  def apply[T <: Data](rv: ReadyValidIO[T], label: String, message: String)(implicit sourceInfo: SourceInfo): Unit = {
    apply( rv.valid &&  rv.ready, label + "_FIRE",  message + ": valid and ready")
    apply( rv.valid && !rv.ready, label + "_STALL", message + ": valid and not ready")
    apply(!rv.valid &&  rv.ready, label + "_IDLE",  message + ": not valid and ready")
    apply(!rv.valid && !rv.ready, label + "_FULL",  message + ": not valid and not ready")
  }
}

object receiver {
  private var propLib: BasePropertyLibrary = new DefaultPropertyLibrary
  def setPropLib(lib: BasePropertyLibrary): Unit = this.synchronized {
    propLib = lib
  }
  def apply(cond: Bool)(implicit sourceInfo: SourceInfo): Unit = {
    propLib.generateProperty(ReceiverPropertyParameters(cond))
  }
  def apply(cond: Bool, label: String)(implicit sourceInfo: SourceInfo): Unit = {
    propLib.generateProperty(ReceiverPropertyParameters(cond, label))
  }
  def apply(cond: Bool, label: String, message: String)(implicit sourceInfo: SourceInfo): Unit = {
    propLib.generateProperty(ReceiverPropertyParameters(cond, label, message))
  }
  def apply(prop: BaseProperty)(implicit sourceInfo: SourceInfo): Unit = {
    prop.generateProperties().foreach( (pp: BasePropertyParameters) => {
      if (pp.pType == MonitorDirection.Receiver) {
        propLib.generateProperty(ReceiverPropertyParameters(pp.cond, pp.label, pp.message))
      }
    })
  }
  def apply[T <: Data](rv: ReadyValidIO[T], label: String, message: String)(implicit sourceInfo: SourceInfo): Unit = {
    apply( rv.valid &&  rv.ready, label + "_FIRE",  message + ": valid and ready")
    apply( rv.valid && !rv.ready, label + "_STALL", message + ": valid and not ready")
    apply(!rv.valid &&  rv.ready, label + "_IDLE",  message + ": not valid and ready")
    apply(!rv.valid && !rv.ready, label + "_FULL",  message + ": not valid and not ready")
  }
}

object monitor {
  private var propLib: BasePropertyLibrary = new DefaultPropertyLibrary
  def setPropLib(lib: BasePropertyLibrary): Unit = this.synchronized {
    propLib = lib
  }
  def apply(cond: Bool)(implicit sourceInfo: SourceInfo): Unit = {
    propLib.generateProperty(MonitorPropertyParameters(cond))
  }
  def apply(cond: Bool, label: String)(implicit sourceInfo: SourceInfo): Unit = {
    propLib.generateProperty(MonitorPropertyParameters(cond, label))
  }
  def apply(cond: Bool, label: String, message: String)(implicit sourceInfo: SourceInfo): Unit = {
    propLib.generateProperty(MonitorPropertyParameters(cond, label, message))
  }
  def apply(prop: BaseProperty)(implicit sourceInfo: SourceInfo): Unit = {
    prop.generateProperties().foreach( (pp: BasePropertyParameters) => {
      if (pp.pType == MonitorDirection.Monitor) {
        propLib.generateProperty(MonitorPropertyParameters(pp.cond, pp.label, pp.message))
      }
    })
  }
  def apply[T <: Data](rv: ReadyValidIO[T], label: String, message: String)(implicit sourceInfo: SourceInfo): Unit = {
    apply( rv.valid &&  rv.ready, label + "_FIRE",  message + ": valid and ready")
    apply( rv.valid && !rv.ready, label + "_STALL", message + ": valid and not ready")
    apply(!rv.valid &&  rv.ready, label + "_IDLE",  message + ": not valid and ready")
    apply(!rv.valid && !rv.ready, label + "_FULL",  message + ": not valid and not ready")
  }
}

// The implementation using a setable global is bad, but removes dependence on Parameters
// This change was made in anticipation of a proper cover library
object cover {
  private var propLib: BasePropertyLibrary = new DefaultPropertyLibrary
  def setPropLib(lib: BasePropertyLibrary): Unit = this.synchronized {
    propLib = lib
  }
  def apply(cond: Bool)(implicit sourceInfo: SourceInfo): Unit = {
    propLib.generateProperty(CoverPropertyParameters(cond))
  }
  def apply(cond: Bool, label: String)(implicit sourceInfo: SourceInfo): Unit = {
    propLib.generateProperty(CoverPropertyParameters(cond, label))
  }
  def apply(cond: Bool, label: String, message: String)(implicit sourceInfo: SourceInfo): Unit = {
    propLib.generateProperty(CoverPropertyParameters(cond, label, message))
  }
  def apply(prop: BaseProperty)(implicit sourceInfo: SourceInfo): Unit = {
    prop.generateProperties().foreach( (pp: BasePropertyParameters) => {
      if (pp.pType == MonitorDirection.Cover) {
        propLib.generateProperty(CoverPropertyParameters(pp.cond, pp.label, pp.message))
      }
    })
  }
  def apply[T <: Data](rv: ReadyValidIO[T], label: String, message: String)(implicit sourceInfo: SourceInfo): Unit = {
    apply( rv.valid &&  rv.ready, label + "_FIRE",  message + ": valid and ready")
    apply( rv.valid && !rv.ready, label + "_STALL", message + ": valid and not ready")
    apply(!rv.valid &&  rv.ready, label + "_IDLE",  message + ": not valid and ready")
    apply(!rv.valid && !rv.ready, label + "_FULL",  message + ": not valid and not ready")
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
                                          override_assn: (T)=>Unit) {
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
    VecInit(apply(in.asUInt).toBools)
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
