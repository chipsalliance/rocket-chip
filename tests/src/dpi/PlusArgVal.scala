package org.chipsalliance.rocketchip.internal.tests.dpi

import chisel3._

class PlusArgVal extends DPIModule {
  val isImport: Boolean = false

  override val exportBody = s"""
                               |function automatic string $desiredName(input string param);
                               |    string val = "";
                               |    if (!$$value$$plusargs(param, val)) val = "";
                               |    return val;
                               |endfunction;
                               |""".stripMargin
}
