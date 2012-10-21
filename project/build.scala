import sbt._
import Keys._

object BuildSettings extends Build {
  val buildOrganization = "berkeley"
  val buildVersion = "1.1"
  val buildScalaVersion = "2.9.2"

  val packageDependencies = TaskKey[Seq[java.io.File]]("package-dependencies", "get package deps")
  val elaborateTask = TaskKey[Unit]("elaborate", "convert chisel components into backend source code")


  val buildSettings = Defaults.defaultSettings ++ Seq (
    //unmanagedBase <<= baseDirectory { base => base / ".." / custom_lib" },
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion,
    elaborateTask <<= fullClasspath in Runtime map {
      (cp: Classpath) => { 
        val projName = "ReferenceChip"
        val dir = "../emulator/generated-src"
        val backend = "c"
        val chiselArgs = Array[String]( "--targetDir", dir, "--backend", backend)
        val classLoader = new java.net.URLClassLoader(cp.map(_.data.toURL).toArray, cp.getClass.getClassLoader)
        val chiselMainClass = classLoader.loadClass("Chisel.chiselMain$")
        val chiselMainObject = chiselMainClass.getDeclaredFields.head.get(null)
        val chiselMain = chiselMainClass.getMethod("run", classOf[Array[String]], classOf[Function0[_]])
        val component = classLoader.loadClass(projName+".Top")
        val generator = () => component.newInstance()
        chiselMain.invoke(chiselMainObject, Array(chiselArgs, generator):_*)
      }
    }

  )


  lazy val chisel = Project("chisel", file("chisel"), settings = buildSettings)
  lazy val hardfloat = Project("hardfloat", file("hardfloat"), settings = buildSettings) dependsOn(chisel)
  lazy val hwacha = Project("hwacha", file("hwacha"), settings = buildSettings) dependsOn(hardfloat,chisel)
  lazy val uncore = Project("uncore", file("uncore"), settings = buildSettings) dependsOn(chisel)
  lazy val rocket = Project("rocket", file("rocket"), settings = buildSettings) dependsOn(uncore,hwacha,hardfloat,chisel)
  lazy val referencechip = Project("referencechip", file("referencechip"), settings = buildSettings) dependsOn(chisel,rocket)



  
  
}
