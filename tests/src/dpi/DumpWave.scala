package org.chipsalliance.rocketchip.internal.tests.dpi

import chisel3._

class DumpWave extends DPIModule {
  val isImport: Boolean = false

  // TODO: think about `chisel3.properties.Property`?
  override val exportBody = s"""
                               |function $desiredName(input string file);
                               |   $$dumpfile(file);
                               |   $$dumpvars(0);
                               |endfunction;
                               |""".stripMargin
}
