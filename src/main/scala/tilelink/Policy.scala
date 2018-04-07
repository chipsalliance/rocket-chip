package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Field
import freechips.rocketchip.rocket.constants.MemoryOpConstants

case object CoherenceKey extends Field[CoherencePolicy]

trait CoherencePolicy {
  // effect, current state, hit?, next state / request param
  val upgradeStart: Seq[(UInt, UInt, Boolean, UInt)]
  // effect, param, next
  val upgradeFinish: Seq[(UInt, UInt, UInt)]
  // wanted, current, hasDirtyData, resp, next
  val downgrade: Seq[(UInt, UInt, Boolean, UInt, UInt)]
}

class MESICoherence extends CoherencePolicy {
  import MemoryOpCategories._
  import TLPermissions._
  import ClientStates._

  val upgradeStart = Seq(
    (rd, Dirty,   true,  Dirty),
    (rd, Trunk,   true,  Trunk),
    (rd, Branch,  true,  Branch),
    (wi, Dirty,   true,  Dirty),
    (wi, Trunk,   true,  Trunk),
    (wr, Dirty,   true,  Dirty),
    (wr, Trunk,   true,  Dirty),
    (rd, Nothing, false, NtoB),
    (wi, Branch,  false, BtoT),
    (wi, Nothing, false, NtoT),
    (wr, Branch,  false, BtoT),
    (wr, Nothing, false, NtoT))

  val upgradeFinish = Seq(
    (rd, toB, Branch),
    (rd, toT, Trunk),
    (wi, toT, Trunk),
    (wr, toT, Dirty))

  val downgrade = Seq(
    (toT, Dirty,   true,  TtoT, Trunk),
    (toT, Trunk,   false, TtoT, Trunk),
    (toT, Branch,  false, BtoB, Branch),
    (toT, Nothing, false, NtoN, Nothing),
    (toB, Dirty,   true,  TtoB, Branch),
    (toB, Trunk,   false, TtoB, Branch), // Policy: Don't notify on clean downgrade
    (toB, Branch,  false, BtoB, Branch),
    (toB, Nothing, false, BtoN, Nothing),
    (toN, Dirty,   true,  TtoN, Nothing),
    (toN, Trunk,   false, TtoN, Nothing), // Policy: Don't notify on clean downgrade
    (toN, Branch,  false, BtoN, Nothing), // Policy: Don't notify on clean downgrade
    (toN, Nothing, false, NtoN, Nothing))
}

class MSICoherence extends CoherencePolicy {
  import MemoryOpCategories._
  import TLPermissions._
  import ClientStates._

  val upgradeStart = Seq(
    (rd, Dirty,   true,  Dirty),
    (rd, Branch,  true,  Branch),
    (wi, Dirty,   true,  Dirty),
    (wr, Dirty,   true,  Dirty),
    (rd, Nothing, false, NtoB),
    (wi, Branch,  false, BtoT),
    (wi, Nothing, false, NtoT),
    (wr, Branch,  false, BtoT),
    (wr, Nothing, false, NtoT))

  val upgradeFinish = Seq(
    (rd, toB, Branch),
    (rd, toT, Dirty),
    (wi, toT, Dirty),
    (wr, toT, Dirty))

  val downgrade = Seq(
    (toT, Dirty,   true,  TtoT, Dirty),
    (toT, Branch,  false, BtoB, Branch),
    (toT, Nothing, false, NtoN, Nothing),
    (toB, Dirty,   true,  TtoB, Branch),
    (toB, Branch,  false, BtoB, Branch),
    (toB, Nothing, false, BtoN, Nothing),
    (toN, Dirty,   true,  TtoN, Nothing),
    (toN, Branch,  false, BtoN, Nothing), // Policy: Don't notify on clean downgrade
    (toN, Nothing, false, NtoN, Nothing))
}

class MICoherence extends CoherencePolicy {
  import MemoryOpCategories._
  import TLPermissions._
  import ClientStates._

  val upgradeStart = Seq(
    (rd, Dirty,   true,  Dirty),
    (wi, Dirty,   true,  Dirty),
    (wr, Dirty,   true,  Dirty),
    (rd, Nothing, false, NtoT),
    (wi, Nothing, false, NtoT),
    (wr, Nothing, false, NtoT))

  val upgradeFinish = Seq(
    (rd, toT, Dirty),
    (wi, toT, Dirty),
    (wr, toT, Dirty))

  val downgrade = Seq(
    (toT, Dirty,   true,  TtoT, Dirty),
    (toT, Nothing, false, NtoN, Nothing),
    (toB, Dirty,   true,  TtoN, Nothing),
    (toB, Nothing, false, BtoN, Nothing),
    (toN, Dirty,   true,  TtoN, Nothing),
    (toN, Nothing, false, NtoN, Nothing))
}
