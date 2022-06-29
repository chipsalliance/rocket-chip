import mill._
import mill.scalalib._
import mill.scalalib.publish._
import coursier.maven.MavenRepository
import mill.define.Sources
import $file.common
import $file.hardfloat.build
import $file.`api-config-chipsalliance`.`build-rules`.mill.build
import os._

object configRocket extends `api-config-chipsalliance`.`build-rules`.mill.build.config with PublishModule {
  override def millSourcePath = os.pwd / "api-config-chipsalliance" / "design" / "craft"

  override def scalaVersion = T {
    rocketchip.scalaVersion()
  }

  override def pomSettings = T {
    rocketchip.`pomSettings`()
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

object compilerrt extends Module {
  override def millSourcePath = os.pwd / "llvm-project" / "compiler-rt"
  // ask make to cache file.
  def compile = T.persistent {
    os.proc("cmake", "-S", millSourcePath,
      "-DCOMPILER_RT_BUILD_LIBFUZZER=OFF",
      "-DCOMPILER_RT_BUILD_SANITIZERS=OFF",
      "-DCOMPILER_RT_BUILD_PROFILE=OFF",
      "-DCOMPILER_RT_BUILD_MEMPROF=OFF",
      "-DCOMPILER_RT_BUILD_ORC=OFF",
      "-DCOMPILER_RT_BUILD_BUILTINS=ON",
      "-DCOMPILER_RT_BAREMETAL_BUILD=ON",
      "-DCOMPILER_RT_INCLUDE_TESTS=OFF",
      "-DCOMPILER_RT_HAS_FPIC_FLAG=OFF",
      "-DCOMPILER_RT_DEFAULT_TARGET_ONLY=On",
      "-DCOMPILER_RT_OS_DIR=riscv64",
      "-DCMAKE_BUILD_TYPE=Release",
      "-DCMAKE_SYSTEM_NAME=Generic",
      "-DCMAKE_SYSTEM_PROCESSOR=riscv64",
      "-DCMAKE_TRY_COMPILE_TARGET_TYPE=STATIC_LIBRARY",
      "-DCMAKE_SIZEOF_VOID_P=8",
      "-DCMAKE_ASM_COMPILER_TARGET=riscv64-none-elf",
      "-DCMAKE_C_COMPILER_TARGET=riscv64-none-elf",
      "-DCMAKE_C_COMPILER_WORKS=ON",
      "-DCMAKE_CXX_COMPILER_WORKS=ON",
      "-DCMAKE_C_COMPILER=clang",
      "-DCMAKE_CXX_COMPILER=clang++",
      "-DCMAKE_C_FLAGS=-nodefaultlibs -fno-exceptions -mno-relax -Wno-macro-redefined -fPIC",
      "-DCMAKE_INSTALL_PREFIX=/usr",
      "-Wno-dev",
    ).call(T.ctx.dest)
    os.proc("make", "-j", Runtime.getRuntime().availableProcessors()).call(T.ctx.dest)
    T.ctx.dest
  }
}

object musl extends Module {
  override def millSourcePath = os.pwd / "musl"
  // ask make to cache file.
  def libraryResources = T.persistent {
    os.proc("make", s"DESTDIR=${T.ctx.dest}", "install").call(compilerrt.compile())
    PathRef(T.ctx.dest)
  }
  def compile = T.persistent {
    val p = libraryResources().path
    os.proc(millSourcePath / "configure", "--target=riscv64-none-elf", "--prefix=/usr").call(
      T.ctx.dest,
      Map (
        "CC" -> "clang",
        "CXX" -> "clang++",
        "AR" -> "llvm-ar",
        "RANLIB" -> "llvm-ranlib",
        "LD" -> "lld",
        "LIBCC" -> "-lclang_rt.builtins-riscv64",
        "CFLAGS" -> "--target=riscv64 -mno-relax -nostdinc",
        "LDFLAGS" -> s"-fuse-ld=lld --target=riscv64 -nostdlib -L${p}/usr/lib/riscv64",
      )
    )
    os.proc("make", "-j", Runtime.getRuntime().availableProcessors()).call(T.ctx.dest)
    T.ctx.dest
  }
}

object pk extends Module {
  override def millSourcePath = os.pwd / "riscv-pk"
  // ask make to cache file.
  def libraryResources = T.persistent {
    os.proc("make", s"DESTDIR=${T.ctx.dest}", "install").call(musl.compile())
    PathRef(T.ctx.dest)
  }
  def compile = T.persistent {
    val p = libraryResources().path
    val env = Map (
      "CC" -> "clang",
      "CXX" -> "clang++",
      "AR" -> "llvm-ar",
      "RANLIB" -> "llvm-ranlib",
      "LD" -> "lld",
      "CFLAGS" -> s"--target=riscv64 -mno-relax -nostdinc -I${p}/usr/include",
      "LDFLAGS" -> "-fuse-ld=lld --target=riscv64 -nostdlib",
    )
    os.proc("autoreconf", "-fi").call(millSourcePath)
    os.proc(millSourcePath / "configure", "--host=riscv64-none-elf").call(T.ctx.dest, env)
    os.proc("make", "-j", Runtime.getRuntime().availableProcessors(), "pk").call(T.ctx.dest, env)
    T.ctx.dest / "pk"
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

  object sanitytests extends Tests with TestModule.ScalaTest {
    override def scalaVersion = T {
      rocketchip.scalaVersion()
    }

    override def moduleDeps: Seq[JavaModule] = Seq(rocketchip)

    override def scalacPluginClasspath = T { super.scalacPluginClasspath() }

    def libraryResources = T.sources {
      val x86Dir = T.ctx.dest
      val riscv64Dir = T.ctx.dest / "riscv64"
      val testDir: Path = os.pwd / "src" / "test" / "scala" / "sanitytests"
      os.copy.into(testDir / "resources", x86Dir)
      def resource(file: String): Path = x86Dir / RelPath(file)
      os.proc("make", s"DESTDIR=${x86Dir}", "install").call(spike.compile())
      os.proc("make", s"DESTDIR=${riscv64Dir}", "install").call(compilerrt.compile())
      os.proc("make", s"DESTDIR=${riscv64Dir}", "install").call(musl.compile())
      os.copy.into(pk.compile(), riscv64Dir)
      // build hello world
      val outputDirectory = x86Dir / "VerilatorTest"
      os.remove.all(outputDirectory)
      os.makeDir(outputDirectory)
      os.proc(
        "clang",
        "-o", "hello",
        s"${resource("resources/csrc/hello.c")}",
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
      val linker = os.pwd / "bootrom" / "linker.ld"
      val bootrom = os.pwd / "bootrom" / "bootrom.S"
      val elf = outputDirectory / "bootrom.elf"
      val bin = outputDirectory / "bootrom.bin"
      val img = outputDirectory / "bootrom.img"
      // format: off
      os.proc(
        "clang",
        "--target=riscv64", "-march=rv64gc",
        "-mno-relax",
        "-static",
        "-nostdlib",
        "-Wl,--no-gc-sections",
        "-fuse-ld=lld", s"-T$linker",
        s"$bootrom",
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
