// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

/** Using e.g. Random.nextInt makes your hardware generation non-repoducible,
  * which is almost certainly undesirable. Use LCG to increment random numbers in HW,
  * or use this SeededRandom.fromSeed to make reproducible Scala PRNGs.
  */
@deprecated("moved to standalone rocketutils library", "rocketchip 2.0.0")
object SeededRandom {
  val fromSeed = new scala.util.Random(42)
}
