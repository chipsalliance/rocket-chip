// See LICENSE.SiFive for license details.

package freechips.rocketchip.aspects

import chisel3.aop.Aspect
import chisel3.RawModule

import firrtl.AnnotationSeq
import firrtl.stage.RunFirrtlTransformAnnotation

import freechips.rocketchip.transforms.naming.{OverrideDesiredNameAnnotation, RenameDesiredNames}

/** An aspect that renames modules
  *
  * @example renaming all Queues in a design to "Queue_$bundleType_entries_$numEntries"
  * {{{
  * case object StabilizeQueueNames extends RenameModulesAspect({ top: RawModule =>
  *   chisel3.aop.Select.collectDeep(top) {
  *     case m: Queue[_] =>
  *       m -> s"Queue_${m.genType.getClass.getSimpleName}_entries_${m.entries}"
  *   }.toSeq
  * })
  * }}}
  *
  * @example renaming a specific instance of RocketCore in the design
  * {{{
  * case object RenameRocketCore0 extends RenameModulesAspect({ top: RawModule =>
  *   chisel3.aop.Select.collectDeep(top) {
  *     case th: TestHarness =>
  *       val core = th.ldut.rocketTiles.head.module.core
  *       core -> "Rocket_Core_0"
  *   }.toSeq
  * })
  * }}}
  *
  * @param collectNameOverrides a function that takes the design top module and returns pairs of name overrides for the modules under the top module in the hierarchy
  */
abstract class RenameModulesAspect(
  collectNameOverrides: RawModule => Seq[(RawModule, String)]
  ) extends Aspect[RawModule] {
  final def toAnnotation(top: RawModule): AnnotationSeq = {
    RunFirrtlTransformAnnotation(new RenameDesiredNames) +: collectNameOverrides(top).map {
      case (m, nameOverride) => OverrideDesiredNameAnnotation(nameOverride, m.toTarget)
    }
  }
}
