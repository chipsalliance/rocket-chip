// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket.fpucp

import chisel3._
import freechips.rocketchip.diplomacy._

package object FPUCP {
  type FPUCPOutwardNode = OutwardNodeHandle[FPUCPNullParameters, FPUCPSinkParameters, FPUCPSinkParameters, FPUCPBundle]
  type FPUCPInwardNode = InwardNodeHandle[FPUCPNullParameters, FPUCPSinkParameters, FPUCPSinkParameters, FPUCPBundle]
  type FPUCPNode = SimpleNodeHandle[FPUCPNullParameters, FPUCPSinkParameters, FPUCPSinkParameters, FPUCPBundle]
}
