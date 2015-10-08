import sbt._
import Keys._
import complete._
import complete.DefaultParsers._

// A class to represent a subproject or a library dependency.
// Cloned from Michael Bayne's post to Coderspiel on 9/29/2011.
class MaybeLocal(locals :(String, String, String, String)*) {
  // If we have a version string, assume it should be treated as a library.
  def isLibrary(version: String) = version != ""

  // Generate project dependencies if we shouldn't use a library.
  def addDeps (p :Project): Project = (locals collect {
    case (id, subp, domain, version) if (!isLibrary(version)) => symproj(file(id), subp)
  }).foldLeft(p) { _ dependsOn _ }
  def addDeps (n: String): Project = addDeps(Project(n, file(n)))

  // Generate library dependencies if we should use a library.
  def libDeps = locals collect {
    case (id, subp, domain, version) if (isLibrary(version)) => domain %% id % version
  }
  private def symproj (dir :File, subproj :String = null) =
    if (subproj == null) RootProject(dir) else ProjectRef(dir, subproj)
}

object BuildSettings extends Build {
  // Return an SBT property value, or the default.
  //  NOTE: Initially this was propOrEnvOrElse and returned
  //  the property setting (if set), or the environment setting,
  //  or the default if neither was set. This is too subtle and can
  //  lead to confusion if subprojects aren't aware of it and use
  //  a different algorithm. or the environment changes from one
  //  project to the next
  def propOrElse(name: String, default: String): String = {
    sys.props.getOrElse(name, sys.env.getOrElse(name, default))
  }
  // Choose either a subproject or a library dependency on chisel
  lazy val chiselFlavor = new MaybeLocal(
    ("chisel", "chisel", "edu.berkeley.cs", propOrElse("chiselVersion", ""))
  )

  override lazy val settings = super.settings ++ Seq(
    organization := "berkeley",
    version      := "1.2",
    scalaVersion := "2.11.6",
    parallelExecution in Global := false,
    traceLevel   := 15,
    scalacOptions ++= Seq("-deprecation","-unchecked"),
    libraryDependencies ++= chiselFlavor.libDeps ++ Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
  )

  lazy val hardfloat = chiselFlavor.addDeps("hardfloat")
  lazy val junctions = chiselFlavor.addDeps("junctions")
  lazy val uncore    = project.dependsOn(junctions)
  lazy val rocket    = project.dependsOn(hardfloat,uncore)
  lazy val zscale    = project.dependsOn(rocket)
  lazy val rocketchip = (project in file(".")).settings(chipSettings).dependsOn(zscale)

  lazy val addons = settingKey[Seq[String]]("list of addons used for this build")
  lazy val make = inputKey[Unit]("trigger backend-specific makefile command")
  val setMake = NotSpace ~ ( Space ~> NotSpace )

  val chipSettings = Seq(
    addons := {
      val a = sys.env.getOrElse("ROCKETCHIP_ADDONS", "")
      println(s"Using addons: $a")
      a.split(",")
    },
    unmanagedSourceDirectories in Compile ++= addons.value.map(baseDirectory.value / _ / "src/main/scala"),
    mainClass in (Compile, run) := Some("rocketchip.TestGenerator"),
    make := {
      val jobs = java.lang.Runtime.getRuntime.availableProcessors
      val (makeDir, target) = setMake.parsed
      (run in Compile).evaluated
      s"make -C $makeDir  -j $jobs $target" !
    }
  )
}
