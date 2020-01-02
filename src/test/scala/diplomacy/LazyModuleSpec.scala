
package freechips.rocketchip.diplomacy

import org.scalatest._

import chisel3._
import chisel3.util.Counter
import chisel3.iotesters._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.unittest._

class LazyModuleSpec extends FlatSpec with Matchers {

  "A simple LazyModule" should "elaborate and generate Verilog" in {
    class SimpleLazyModule extends LazyModule()(Parameters.empty) {
      lazy val module = new LazyModuleImp(this) {
        val io = IO(new Bundle {
          val in = Input(UInt(8.W))
          val out = Output(UInt(8.W))
        })
        io.out := io.in
      }
    }

    class SimpleUnitTest extends UnitTest(10) {
      val inst = Module(LazyModule(new SimpleLazyModule).module)
      inst.io.in := 3.U
      io.finished := true.B
    }

    iotesters.Driver(() => new SimpleUnitTest) {
      c => new UnitTestTester(c)
    } should be (true)
  }

  // Inspired by AsyncResetTester in chisel3 tests
  "LazyRawModuleImps" should "be able to have child LazyModules in reset domains of different types" in {
    class RegLazyModule extends LazyModule()(Parameters.empty) {
      lazy val module = new LazyModuleImp(this) {
        val io = IO(new Bundle {
          val out = Output(UInt(8.W))
        })
        val reg = RegInit(123.U)
        reg := 5.U
        io.out := reg
      }
    }

    // Creates two reset domains, one sync, one async
    class ResetDomainWrapper extends LazyModule()(Parameters.empty) {
      val syncChild = LazyModule(new RegLazyModule)
      val asyncChild = LazyModule(new RegLazyModule)

      lazy val module = new LazyRawModuleImp(this) {
        val io = IO(new Bundle {
          val reset         = Input(Bool())
          val slowClock     = Input(Clock())
          val syncRegValue  = Output(UInt(8.W))
          val asyncRegValue = Output(UInt(8.W))
        })
        childClock := io.slowClock
        // Create reset domains of different types
        syncChild.module.reset  := io.reset
        asyncChild.module.reset := io.reset.asAsyncReset
        // Propagate child outputs
        io.syncRegValue  := syncChild.module.io.out
        io.asyncRegValue := asyncChild.module.io.out
      }
    }

    class ResetDomainTest(timeout: Int = 16) extends UnitTest(timeout) {

      val (_, clockDivider) = Counter(true.B, 4)
      // First rising edge when count === 3
      val slowClock = clockDivider.asClock

      val (count, done) = Counter(true.B, 16)

      val resetNext = RegInit(false.B)
      resetNext := count === 4.U

      val wrapper = Module(LazyModule(new ResetDomainWrapper).module)
      wrapper.io.reset := resetNext
      wrapper.io.slowClock := slowClock

      // Check that simulation environment resets everything
      when (count === 3.U) {
        chisel3.assert(wrapper.io.syncRegValue === 5.U)
        chisel3.assert(wrapper.io.asyncRegValue === 5.U)
      }
      // Check that async reset occurs
      when (count >= 5.U && count < 7.U) {
        chisel3.assert(wrapper.io.syncRegValue === 5.U)
        chisel3.assert(wrapper.io.asyncRegValue === 123.U)
      }
      // Check that reset ends
      .elsewhen (count >= 7.U) {
        chisel3.assert(wrapper.io.syncRegValue === 5.U)
        chisel3.assert(wrapper.io.asyncRegValue === 5.U)
      }

      io.finished := false.B
      when (done) {
        io.finished := true.B
      }
    }

    val args = Array("--backend-name", "verilator")
    iotesters.Driver.execute(args, () => new ResetDomainTest) {
      c => new UnitTestTester(c)
    } should be (true)
  }
}
