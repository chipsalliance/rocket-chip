package groundtest

import Chisel._
import cde.Parameters

// !!! TODO: Replace with a groundtest-specific test harness
class TestHarness(implicit p: Parameters) extends rocketchip.TestHarness()(p)
