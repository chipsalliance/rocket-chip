package diplomaticobjectmodel.model

import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent

case class OMCoreComplex(
  components: Seq[OMComponent],
  documentationName: String
) extends OMComponent
