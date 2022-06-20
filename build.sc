import $file.common
import $file.hardfloat.build
import $file.`api-config-chipsalliance`.`build-rules`.mill.build
import os.{Path, proc, resource}

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

object spike extends Module {
  override def millSourcePath = os.pwd / "riscv-isa-sim"
  def compile = T.persistent {
    os.proc(millSourcePath / "configure", "--prefix", "/usr", "--without-boost", "--without-boost-asio", "--without-boost-regex").call(
      T.ctx.dest, Map(
        "CC" -> "clang",
        "CXX" -> "clang++",
        "AR" -> "llvm-ar",
        "RANLIB" -> "llvm-ranlib",
        "LD" -> "lld",
      )
    )
    os.proc("make", "-j", Runtime.getRuntime().availableProcessors()).call(T.ctx.dest)
    T.ctx.dest
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

  object sanitytests extends ScalaModule {
    override def scalaVersion = T {
      rocketchip.scalaVersion()
    }

    override def moduleDeps: Seq[JavaModule] = Seq(rocketchip)

    override def scalacPluginClasspath = T { super.scalacPluginClasspath() }

    def resource(file: String): Path = Path(java.nio.file.Paths.get(getClass().getClassLoader().getResource(file).toURI))

    def libraryResources = T.sources {
      val x86Dir = T.ctx.dest
      // build spike
      os.proc("make", s"DESTDIR=${x86Dir}", "install").call(spike.compile())
      // build hello world
      val outputDirectory = os.pwd / "out" / "SanityTest"
      os.remove.all(outputDirectory)
      os.makeDir(outputDirectory)
      os.proc(
        "clang",
        "-o", "hello",
        s"${resource("csrc/hello.c")}",
        "--target=riscv64",
        "-mno-relax",
        "-nostdinc",
        s"-I${resource("riscv64/usr/include")}",
        "-fuse-ld=lld",
        "-nostdlib",
        s"${resource("riscv64/usr/lib/crt1.o")}",
        s"${resource("riscv64/usr/lib/crti.o")}",
        s"${resource("riscv64/usr/lib/riscv64/libclang_rt.builtins-riscv64.a")}",
        s"${resource("riscv64/usr/lib/libc.a")}",
        s"${resource("riscv64/usr/lib/crtn.o")}",
        "-static",
      ).call(outputDirectory)
      // build bootrom
      val tmp = os.temp.dir()
      val elf = tmp / "bootrom.elf"
      val bin = tmp / "bootrom.bin"
      val img = tmp / "bootrom.img"
      // format: off
      os.proc(
        "clang",
        "--target=riscv64", "-march=rv64gc",
        "-mno-relax",
        "-static",
        "-nostdlib",
        "-Wl,--no-gc-sections",
        "-fuse-ld=lld", s"-T${resource("linker.ld")}",
        s"${resource("bootrom.S")}",
        "-o", elf
      ).call()
      os.proc(
        "llvm-objcopy",
        "-O", "binary",
        elf,
        bin
      ).call()
      os.proc(
        "dd",
        s"if=$bin",
        s"of=$img",
        "bs=128",
        "count=1"
      ).call()
      T.ctx.dest
    }
    override def resources: Sources = T.sources {
      super.resources() ++ libraryResources()
    }
  }
}
