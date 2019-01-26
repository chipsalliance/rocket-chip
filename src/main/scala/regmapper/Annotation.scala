// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import chisel3.experimental.{ChiselAnnotation, RawModule, RunFirrtlTransform}
import firrtl.annotations._
import firrtl.{CircuitForm, CircuitState, LowForm, Transform}

case class RegFieldDescSer(
  byteOffset: String,
  bitOffset: Int,
  bitWidth: Int,
  name: String,
  resetValue: BigInt,
  accessType: String,
  wrType: String,
  rdAction: String,
  desc: String,
  group: String,
  groupDesc: String,
  volatile: Boolean = false,
  hasReset: Boolean = false,
  enumerations: Map[BigInt, (String, String)] = Map()
)

case class RegistersSer(
  displayName: String,
  deviceName: String,
  baseAddress: BigInt,
  regFields: Seq[RegFieldDescSer]
)
