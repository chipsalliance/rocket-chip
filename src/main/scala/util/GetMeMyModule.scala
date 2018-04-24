package chisel3.internal

/*
* GetMeMyModule is a hack to get the current module during elaboration.
* A pull request will be issued against Chisel3 to add this function to:
* chiselFrontend/src/main/scala/chisel3/core/Module.scala:79
* The pull request adds the following function:
* /** Returns the current elaborating module */
* def self: Option[BaseModule] = Builder.currentModule
* When and if the function is added to Chisel3 GetMeMyModule should be deleted
* and replaced with the Chisel:Module.self call
*/

object GetMeMyModule {
  def currentModule: Option[chisel3.core.BaseModule] = Builder.currentModule
}

