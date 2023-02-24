// SPDX-License-Identifier: Apache-2.0
// hack internal converter to use convertLazily
package chisel3
package hack

import chisel3.internal.firrtl.Converter
import chisel3.stage.ChiselCircuitAnnotation
import firrtl.AnnotationSeq
import firrtl.options.Phase
import firrtl.stage.{FirrtlCircuitAnnotation, RunFirrtlTransformAnnotation}

/** This prepares a [[ChiselCircuitAnnotation]] for compilation with FIRRTL. This does three things:
 *   - Uses [[chisel3.internal.firrtl.Converter]] to generate a [[FirrtlCircuitAnnotation]]
 *   - Extracts all [[firrtl.annotations.Annotation]]s from the [[chisel3.internal.firrtl.Circuit]]
 *   - Generates any needed [[RunFirrtlTransformAnnotation]]s from extracted [[firrtl.annotations.Annotation]]s
 */
class Convert extends Phase {
  def transform(annotations: AnnotationSeq): AnnotationSeq = annotations.flatMap {
    case a: ChiselCircuitAnnotation =>
      /* Convert this Chisel Circuit to a FIRRTL Circuit */
      Some(FirrtlCircuitAnnotation(Converter.convertLazily(a.circuit))) ++
        /* Convert all Chisel Annotations to FIRRTL Annotations */
        a.circuit.firrtlAnnotations
    case a => Some(a)
  }
}