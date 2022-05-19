import mill._
import mill.scalalib._
import mill.scalalib.publish._
import coursier.maven.MavenRepository
import $file.common
import $file.hardfloat.build
import $file.`api-config-chipsalliance`.`build-rules`.mill.build
import mill.define.Sources

object configRocket extends `api-config-chipsalliance`.`build-rules`.mill.build.config with PublishModule {
  override def millSourcePath = os.pwd / "api-config-chipsalliance" / "design" / "craft"

  override def scalaVersion = T {
    rocketchip.scalaVersion()
  }

  override def pomSettings = T {
    rocketchip.pomSettings()
  }

  override def publishVersion = T {
    rocketchip.publishVersion()
  }
}

object hardfloatRocket extends hardfloat.build.hardfloat {
  override def millSourcePath = os.pwd / "hardfloat"

  override def scalaVersion = T {
    rocketchip.scalaVersion()
  }

  // use same chisel version with RocketChip
  def chisel3IvyDeps = if(chisel3Module.isEmpty) Agg(
    common.getVersion("chisel3")
  ) else Agg.empty[Dep]
}

object sanitytests extends ScalaModule with TestModule.Utest {
  override def scalaVersion = T {
    rocketchip.scalaVersion()
  }

  override def moduleDeps: Seq[JavaModule] = Seq(rocketchip)

  override def scalacPluginClasspath = T { super.scalacPluginClasspath() }

  object ivys {
    val chisel3 = ivy"edu.berkeley.cs::chisel3:3.5.3"
    val firrtl = ivy"edu.berkeley.cs::firrtl_2.13:1.5.3"
    val scalatest = ivy"org.scalatest::scalatest:3.2.0"
    val oslib = ivy"com.lihaoyi::os-lib:0.7.8"
    val utest = ivy"com.lihaoyi::utest:0.7.10"
  }

  override def ivyDeps = Agg(
    ivys.chisel3,
    ivys.scalatest,
    ivys.oslib,
    ivys.utest
  )

  object helper {
    val isMac = System.getProperty("os.name").toLowerCase.startsWith("mac")
  }

  def libraryResources = T.sources {
    //    if (!helper.isMac) {
    //      val x86Dir = T.ctx.dest
    //      val riscv64Dir = T.ctx.dest / "riscv64"
    //      os.proc("make", s"DESTDIR=${x86Dir}", "install").call(spike.compile())
    //      os.proc("make", s"DESTDIR=${riscv64Dir}", "install").call(compilerrt.compile())
    //      os.proc("make", s"DESTDIR=${riscv64Dir}", "install").call(musl.compile())
    //      os.copy.into(pk.compile(), riscv64Dir)
    //    }
    T.ctx.dest
  }
  override def resources: Sources = T.sources {
    super.resources() ++ libraryResources()
  }
}

object rocketchip extends common.CommonRocketChip {
  m =>
  override def scalaVersion: T[String] = T {
    "2.12.10"
  }
  override def ammoniteVersion: T[String] = T {
    "2.4.0"
  }

  def hardfloatModule = hardfloatRocket

  def configModule = configRocket

  def testModule = sanitytests
}
