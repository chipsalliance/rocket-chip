package chisel3

/**
  * The mixin to define the probe signal for BlackBox.
  */
trait HasExtModuleDefine extends chisel3.util.HasExtModuleInline { this: chisel3.experimental.ExtModule =>

  /** Create a ProbeType for external sources.
    * @param tpe is the Chisel type of signal.
    * @param path is the internal path of the signal see [[https://github.com/chipsalliance/firrtl-spec/blob/main/abi.md]]
    */
  def define[T <: chisel3.Element](tpe: T, path: Seq[String]): T = {
    setInline(
      s"ref_${path.mkString("_")}.sv",
      s"`define ref_${path.mkString("_")} ${path.last}"
    )
    // it's not IO, but BlackBox can only bind IO
    val io = IO(tpe).suggestName(path.last)
    require(chisel3.reflect.DataMirror.hasProbeTypeModifier(io), s"$tpe for $path should be a ProbeType")
    io
  }

  def defineVec[A <: chisel3.Vec[_]](tpe: A, path: Seq[String]): A = {
    setInline(
      s"ref_${path.mkString("_")}.sv",
      (Seq(s"`define ref_${path.mkString("_")} ${path.last}")
        ++ Seq.tabulate(tpe.size)(i => s"`define ref_${path.mkString("_")}_$i ${path.last}[$i]")).mkString("\n")
    )
    // it's not IO, but BlackBox can only bind IO
    val io = IO(tpe).suggestName(path.last)
    require(chisel3.reflect.DataMirror.hasProbeTypeModifier(io), s"$tpe for $path should be a ProbeType")
    io
  }
}
