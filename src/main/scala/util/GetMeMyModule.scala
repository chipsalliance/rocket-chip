package chisel3.internal

object GetMeMyModule {
  def currentModule: Option[chisel3.core.BaseModule] = Builder.currentModule
}

