import sbt._
import Keys._
import scala.language.postfixOps
//val extracted: Extracted = Project.extract(state)
//import extracted._

object BuildSettings extends Build {
  val buildOrganization = "berkeley"
  val buildVersion = "1.1"
  val buildScalaVersion = "2.10.2"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    //unmanagedBase <<= baseDirectory { base => base / ".." / custom_lib" },
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion,
    parallelExecution in Global := false,
    traceLevel   := 15,
    scalacOptions ++= Seq("-deprecation","-unchecked"),
    libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
    //                        "edu.berkeley.cs" %% "chisel" % "2.3-SNAPSHOT",
    //                        "edu.berkeley.cs" %% "hardfloat" % "1.2"),
    //resolvers ++= Seq(
    //  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
    //  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
    //  "scct-github-repository" at "http://mtkopone.github.com/scct/maven-repo"
    //)
  )

  lazy val chisel     = Project("chisel", file("chisel"), settings = buildSettings)
  lazy val hardfloat  = Project("hardfloat", file("hardfloat"), settings = buildSettings) dependsOn(chisel)
  lazy val uncore     = Project("uncore", file("uncore"), settings = buildSettings) dependsOn(hardfloat)
  lazy val rocket     = Project("rocket", file("rocket"), settings = buildSettings) dependsOn(uncore)
  lazy val zscale     = Project("zscale", file("zscale"), settings = buildSettings) dependsOn(rocket)

  val baselist = Vector("chisel", "uncore", "rocket", "zscale", "hardfloat")
  def getsubdirs = {
    val blacklist = (baselist ++ Vector("target", "project"))
    IO.listFiles(file(".")) map (_.toString.split("/").last) filter (f=> !blacklist.contains(f) && (f(0)!='.')) filter (f=> !IO.listFiles(file(f+"/src/main/scala")).isEmpty)
  }
  val addonsources = getsubdirs map (f=>s"${f}/src/main/scala") map (f=>file(f))
  addonsources.foreach(a => println(s"[info] Found addon: " + a.toString.split("/").head))

  val prepareTask = TaskKey[Unit]("prepare","Remove old sources and copy over new ones to addon/src")
  def prepareTaskImpl = {
    import IO._
    delete(file(".addons-dont-touch/src"))
    createDirectory(file(".addons-dont-touch/src/main/scala"))
    addonsources.foreach(as => {
      val addonname = as.toString.split("/").head
      copyDirectory(as, file(".addons-dont-touch/src/main/scala/"+addonname))
    })
  }
  
  lazy val addons     = Project("addons", file(".addons-dont-touch"), settings = buildSettings ++ Seq(
    prepareTask := prepareTaskImpl,
    (compile in Compile) <<= (compile in Compile) dependsOn (prepareTask)
  )) dependsOn(chisel, hardfloat, uncore, rocket, zscale)
  lazy val rocketchip = Project("rocketchip", file("."), settings = buildSettings ++ chipSettings).dependsOn(chisel, uncore, rocket, zscale, addons)

  val elaborateTask = InputKey[Unit]("elaborate", "convert chisel components into backend source code")
  val makeTask = InputKey[Unit]("make", "trigger backend-specific makefile command")

  def runChisel(args: Seq[String], cp: Classpath, pr: ResolvedProject) = {
     val numArgs = 1
     require(args.length >= numArgs, "syntax: elaborate <component> [chisel args]")
     val projectName = pr.id
     val packageName = projectName //TODO: valid convention?
     val componentName = args(0)
     val classLoader = new java.net.URLClassLoader(cp.map(_.data.toURI.toURL).toArray, cp.getClass.getClassLoader)
     val chiselMainClass = classLoader.loadClass("Chisel.chiselMain$")
     val chiselMainObject = chiselMainClass.getDeclaredFields.head.get(null)
     val chiselMain = chiselMainClass.getMethod("run", classOf[Array[String]], classOf[Function0[_]])
     val chiselArgs = args.drop(numArgs)
     val component = classLoader.loadClass((if (!componentName.contains('.')) packageName+"." else "")+componentName)
     val generator = () => component.newInstance()
     chiselMain.invoke(chiselMainObject, Array(chiselArgs.toArray, generator):_*)
  }

  val chipSettings = Seq(
    elaborateTask <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
      (argTask, fullClasspath in Runtime, thisProject) map {
        runChisel
      }
    },
    makeTask <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
      (argTask, fullClasspath in Runtime, thisProject) map {
        (args: Seq[String], cp: Classpath, pr: ResolvedProject) => {
          require(args.length >= 2, "syntax: <dir> <target>")
          runChisel(args.drop(2), cp, pr)
          val makeDir = args(0)
          val target = args(1)
          val jobs = java.lang.Runtime.getRuntime.availableProcessors
          val make = "make -C" + makeDir + " -j" + jobs + " " + target
          make!
        }
      }
    }
  )
}
