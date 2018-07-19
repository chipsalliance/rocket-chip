// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

abstract class CrossingHelper(parent: LazyModule with LazyScope, name: String) {

  // Detect incorrect crossing connectivity
  protected def crossingCheck(out: Boolean, source: BaseNode, sink: BaseNode) {
    InModuleBody {
      def inside(node: BaseNode) = node.parents.exists(_ eq parent)
      source.inputs.foreach { case (syncSource, _) =>
        require (inside(syncSource) == out, s"${syncSource.name} source must ${if(out)""else"not "}be inside ${parent.name} (wrong .cross direction?)")
      }
      sink.outputs.foreach { case (syncSink, _) =>
        require (inside(syncSink) != out, s"${syncSink.name} sink must ${if(out)"not "else""}be inside ${parent.name} (wrong .cross direction?)")
      }
    }
  }
}
