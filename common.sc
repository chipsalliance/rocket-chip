import mill._
import mill.scalalib._
import mill.scalalib.publish._
import coursier.maven.MavenRepository

val defaultVersions = Map(
  "chisel3" -> "3.4.0",
  "chisel3-plugin" -> "3.4.0"
)

def getVersion(dep: String, org: String = "edu.berkeley.cs", cross: Boolean = false) = {
  val version = sys.env.getOrElse(dep + "Version", defaultVersions(dep))
  if (cross)
    ivy"$org:::$dep:$version"
  else
    ivy"$org::$dep:$version"
}

/** The reason to split build.sc to two file is
  * [[CommonRocketChip]] doesn't need to import `$file.chisel3` and `$file.firrtl`.
  *
  * You can extends from [[CommonRocketChip]] to use rocket-chip as build-from-source dependency.
  * When doing this, you may like to override `chisel3Module`, `hardfloatModule`, `diplomacyModule`,
  * setting them to your favorite commit of those packages.
  *
  * If you don't override `chisel3Module`, which will default to be `None`,
  * and mill will automatically use chisel3 from ivy.
  */
trait CommonRocketChip extends SbtModule with PublishModule {
  m =>
  object test extends Tests {
    override def scalacPluginClasspath = m.scalacPluginClasspath

    override def ivyDeps = m.ivyDeps() ++ Agg(
      ivy"org.scalatest::scalatest:3.2.0"
    )

    def testFrameworks = T {
      Seq("org.scalatest.tools.Framework")
    }

    // a sbt-like testOnly command.
    // for example, mill -i "chisel3[2.12.12].test.testOnly" "chiselTests.BitwiseOpsSpec"
    def testOnly(args: String*) = T.command {
      super.runMain("org.scalatest.run", args: _*)
    }
  }

  override def millSourcePath = super.millSourcePath / os.up

  def chisel3Module: Option[PublishModule] = None

  def hardfloatModule: PublishModule

  def diplomacyModule: PublishModule

  def utilityModule: PublishModule

  def chisel3IvyDeps = if (chisel3Module.isEmpty) Agg(
    getVersion("chisel3")
  ) else Agg.empty[Dep]

  override def mainClass = T {
    Some("freechips.rocketchip.system.Generator")
  }

  override def moduleDeps = Seq() ++ chisel3Module :+ hardfloatModule :+ diplomacyModule :+ utilityModule

  override def scalacOptions = T {
    Seq("-deprecation", "-unchecked", "-Xsource:2.11")
  }

  override def ivyDeps = T {
    Agg(
      ivy"${scalaOrganization()}:scala-reflect:${scalaVersion()}",
      ivy"org.json4s::json4s-jackson:3.6.1",
      ivy"org.scalatest::scalatest:3.2.0"
    ) ++ chisel3IvyDeps
  }

  private val macroParadise = ivy"org.scalamacros:::paradise:2.1.1"

  private val chisel3Plugin = getVersion("chisel3-plugin")

  override def repositories = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots"),
    MavenRepository("https://oss.sonatype.org/content/repositories/releases")
  )

  override def compileIvyDeps = Agg(macroParadise)

  override def scalacPluginIvyDeps = Agg(macroParadise) ++ (if(chisel3Module.isDefined) Agg() else Agg(chisel3Plugin))

  def publishVersion = T {
    "1.2-SNAPSHOT"
  }

  def pomSettings = T {
    PomSettings(
      description = artifactName(),
      organization = "edu.berkeley.cs",
      url = "https://github.com/chipsalliance/rocket-chip",
      licenses = Seq(License.`Apache-2.0`, License.`BSD-3-Clause`),
      versionControl = VersionControl.github("chipsalliance", "rocket-chip"),
      developers = Seq.empty
    )
  }
  override def artifactName = "rocketchip"
}
