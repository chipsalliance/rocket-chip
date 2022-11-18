// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import chisel3._
import chisel3.util.BitPat
import chisel3.util.log2Ceil
import chisel3.util.experimental.decode._
import freechips.rocketchip.diplomacy.{AddressDecoder, AddressSet}

// import chisel3.stage.ChiselGeneratorAnnotation
// import circt.stage.{CIRCTTarget, CIRCTTargetAnnotation, ChiselStage, FirtoolOption}
// import firrtl.options.TargetDirAnnotation
// import firrtl.{AnnotationSeq, ChirrtlEmitter, EmitAllModulesAnnotation}

object EspressoAddressDecoder
{
	type Port = Seq[AddressSet]
	type Ports = Seq[Port]

	def apply(addr: UInt, ports: Ports, mask: Seq[Boolean]): Seq[Bool] = {
		if (ports.flatten.isEmpty) {
			return ports.map(_ => true.B)
		}
		// Verify the user did not give us an impossible problem
		ports.combinations(2).foreach { case Seq(x, y) =>
			x.foreach { a =>
				y.foreach { b =>
					require(!a.overlaps(b), s"Ports cannot overlap: $a $b")
				}
			}
		}
		val maxBits = log2Ceil(1 + ports.flatMap(_.map(_.base)).max)
		// val maxBits = 32

		val truthMap = {
			ports.zipWithIndex.zip(mask).flatMap {
				case ((port, i), false) =>
					port.map { range =>
						(AddressSetToBitPat(range, maxBits), new BitPat(value = 1 << i, mask = -1, maxBits))
					}
				case _ => Seq()
			}
		}
		if (truthMap.isEmpty) {
			return ports.map(_ => true.B)
		}
		val dontCarePat: BitPat = new BitPat(value = 0x0, mask = 0x0, maxBits)
		decoder(addr, TruthTable(truthMap, dontCarePat)).asBools.take(ports.length)
	}

	def AddressSetToBitPat(addrs: AddressSet, XLen: Int): BitPat = {
		new BitPat(addrs.base, ~addrs.mask, XLen)
	}
}
/*
abstract class Decoder(ports: Int)(implicit XLen: Int) extends Module {
	val addr = IO(Input(UInt(XLen.W)))
	val port = IO(Output(Vec(ports, Bool())))
	implicit class AddressBitPatBuilder(addrs: AddressSet) {
		def asBitPat(implicit XLen: Int): BitPat = {
			new BitPat(addrs.base, ~addrs.mask, XLen)
		}
	}
}

class EspressoDecoder(ports: Seq[Seq[AddressSet]])(implicit XLen: Int) extends Decoder(ports.length) {
	private val truthMap = {
		ports.zipWithIndex.flatMap { case (port, i) =>
			port.map { range =>
				(range.asBitPat, new BitPat(value= 1<<i, mask = -1, XLen))
			}
		}
	}
	private val dontCarePat: BitPat = new BitPat(value=0x0, mask=0x0, XLen)

	val decoderOut = Wire(UInt(ports.length.W))
	decoderOut := decoder(addr, TruthTable(truthMap, dontCarePat))
	port := decoderOut.asBools()
}

class OrigialDecoder(ports: Seq[Seq[AddressSet]])(implicit XLen: Int) extends Decoder(ports.length) {
	private val routeMask = AddressDecoder(ports)
	private val routeAddrs = ports.map {s => AddressSet.unify(s.map(_.widen(~routeMask)).distinct)}
	private val portFuncs = routeAddrs.map(s=>(addr: UInt) => s.map(_.contains(addr)).reduce(_ || _))

	port:= portFuncs.map(_(addr))
}

object OrigialDecoder
{
	def apply(ports: Seq[Seq[AddressSet]]) extends Decoder(ports.length) {
		private val routeMask = AddressDecoder(ports)
		private val routeAddrs = ports.map {s => AddressSet.unify(s.map(_.widen(~routeMask)).distinct)}
		private val portFuncs = routeAddrs.map(s=>(addr: UInt) => s.map(_.contains(addr)).reduce(_ || _))

		port:= portFuncs.map(_(addr))
	}
}

class DecoderTop extends Module {
	implicit val XLen: Int = 32
	val addr = IO(Input(UInt(XLen.W)))
	val result = IO(Output(Vec(3, UInt(5.W))))

	private val addressMap = Seq(
		Seq(AddressSet(BigInt("20000000",16), 0x1FFFFFFF), AddressSet(BigInt("80000000",16), 0x1FFFFFFF)),
		Seq(AddressSet(BigInt("40000000",16), 0x00000FFF)),
		Seq(AddressSet(BigInt("40001000",16), 0x00000FFF)),
		Seq(AddressSet(BigInt("40002000",16), 0x00000FFF)),
		Seq(AddressSet(BigInt("40010000",16), 0x00000FFF))
	)

	val origialDecoder = Module(OrigialDecoder(addressMap))
	val espressoDecoder = Module(EspressoDecoder(addressMap))

	val addr_reg = RegNext(addr)
	val result_reg = Reg(Vec(2, UInt(5.W)))

	origialDecoder.addr := addr_reg
	espressoDecoder.addr := addr_reg

	result_reg(0) := origialDecoder.port.asUInt()
	result_reg(1) := espressoDecoder.port.asUInt()
	print(result_reg)
	result := result_reg
}

object Main extends app {
	def elaborate(): Unit = {
		(new ChiselStage).emitSystemVerilog(
			new DecoderTop,
			Array("--target-dir", "generated")
		)
	}

	def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}

val addr1 = AddressSet(BigInt(0x0000), 0x0FF)
val addr2 = AddressSet(BigInt(0x0F00), 0x0FF)
val addr3 = AddressSet(BigInt(0x2000), 0x0FF)

val portAddrs = Seq(Seq(addr1), Seq(addr2), Seq(addr3))
val addrMask = AddressDecoder(portAddrs)
println(addrMask.toString(16))

val routeAddrs = portAddrs.map { seq => AddressSet.unify(seq.map(_.widen(~addrMask)).distinct)}
val outputPorts = routeAddrs.map {
	seq => (addr: UInt) => seq.map(_.contains(addr)).reduce(_ || _)
}

val addrTable = TruthTable(
	table = Map(
		(new BitPat(value=0x0, mask=0xff00, width=16), BitPat("b0001")),
		(new BitPat(value=0xf00, mask=0xff00, width=16), BitPat("b0010")),
		(new BitPat(value=0x2000, mask=0xff00, width=16), BitPat("b0100")),
		default = BitPat("b1000")		
	)
)

class mod(outputPorts: Seq[UInt => Bool]) extends Module {
	val addr = IO(Input(UInt(16.W)))
	val res1 = IO(Output(UInt(outputPorts.length.W)))
	val res2 = IO(Output(UInt(4.W)))

	val raw = Wire(Vec(outputPorts.length, Bool()))
	raw := outputPorts.map{o => o(addr)}
	res1 := raw.asUInt()
}

*/
