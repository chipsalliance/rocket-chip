package org.chipsalliance.rocketchip.internal.tests.dpi

import chisel3._

class Finish extends DPIModule {
  val isImport: Boolean = false
  override val exportBody = s"""
                               |function $desiredName();
                               |   $$finish;
                               |endfunction;
                               |""".stripMargin
}
