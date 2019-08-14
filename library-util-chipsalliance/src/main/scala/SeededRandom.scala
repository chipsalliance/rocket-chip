// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

/** Using e.g. Random.nextInt makes your hardware generation non-repoducible,
  * which is almost certainly undesirable. Use LCG to increment random numbers in HW,
  * or use this SeededRandom.fromSeed to make reproducible Scala PRNGs.
  */
object SeededRandom {
  val fromSeed = new scala.util.Random(42)
}
