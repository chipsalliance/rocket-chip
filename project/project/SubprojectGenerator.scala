import sbt._
import chiselBuild.ChiselSettings
import chiselBuild.ChiselDependencies.{PackageProject, basicDependencies}

object SubprojectGenerator {

  def generate(output: File, projects: Seq[PackageProject]): Seq[File] = {
    val subappsString = projects.map(_.packageName).mkString(",")
    val packageProjects: scala.collection.immutable.Set[String] = projects.map(_.packageName).toSet

    // For a given chisel project, return a sequence of project references,
    //  suitable for use as an argument to dependsOn().
    def projectDependencies(name: String): Seq[String] = {
      basicDependencies(name).filter(dep => packageProjects.contains(dep))
    }

    // Given a Chisel project, generate the sbt code to define it as a project.
    def projFromPackageProject(p: PackageProject) = {
      val clientSettings = p.settings.getOrElse(Seq())
      val id = p.packageName
      val safeId = ChiselSettings.safeScalaIdentifier(id)
      val base = p.base.getOrElse(file(id)).name
      val projectDependenciesString = projectDependencies(id).map(ChiselSettings.safeScalaIdentifier(_)).mkString(", ")
      s"""
        |    lazy val $safeId = (project in file(\"$base\")).settings(
        |      $clientSettings ++ ChiselSettings.commonSettings ++ ChiselSettings.publishSettings ++ Seq(
        |        libraryDependencies ++= chiselLibraryDependencies(Seq("$id"))
        |      )
        |    ).dependsOn($projectDependenciesString)
        |""".stripMargin
    }

    val packageProjectsBuildString = projects.map(projFromPackageProject).mkString("\n")
    val packageProjectsInitString = projects.map( p => s"""      "${p.packageName}" -> ${ChiselSettings.safeScalaIdentifier(p.packageName)}""").mkString(",\n")
    val source = s"""
        |import sbt._
        |import Keys._
        |import chiselBuild.ChiselSettings
        |import chiselBuild.ChiselDependencies._
        |
        |object SubprojectBuild extends Build {
        |
        |$packageProjectsBuildString
        |
        |    packageProjectsMap = scala.collection.immutable.Map[String, ProjectReference](
        |$packageProjectsInitString
        |    )
        |}
        |""".stripMargin

    val outputFile = output / "Subprojects.scala"

    println("generate: " + outputFile.getPath)

    IO.write(outputFile,source)

    Seq(outputFile)
  }

}
