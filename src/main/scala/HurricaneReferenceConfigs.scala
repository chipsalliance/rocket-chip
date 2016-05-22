package rocketchip

import Chisel._
import uncore._
import rocket._
import hwacha._
import cde.{Parameters, Config, Dump, Knob, Field}

class With2L2AcquireXacts extends Config(
  (pname,site,here) => pname match {
    case NAcquireTransactors => 2
  }
)

class Hurricane1ReferenceConfig extends Config(new With2Cores ++ new With2L2AcquireXacts ++ new WithL2Capacity256 ++ new With4BanksPerMemChannel ++ new EOS24Config)
