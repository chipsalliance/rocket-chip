// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting

import firrtl._
import firrtl.ir._
import scala.collection.mutable

package object rule {
  /** Determines whether name is prepended with an underscore, indicating a bad name
    *
    * @param name a signal's name
    */
  private [linting] def isTemporary(name: String): Boolean = name.nonEmpty && name.head == '_'

  /** Determines whether name is prepended with an underscore, indicating a bad name
    *
    * @param expr a expression of a signal name
    */
  private [linting] def isTemporary(expr: Expression): Boolean = isTemporary(getName(expr))

  /** Returns the root reference name of an Expression
    *
    * @throws Exception
    * @param expr an expression of a signal name. Cannot contain DoPrims or Muxes etc.
    */
  private [linting] def getName(expr: Expression): String = expr match {
    case r: WRef => r.name
    case f: WSubField => getName(f.expr)
    case i: WSubIndex => getName(i.expr)
    case a: WSubAccess => getName(a.expr)
    case other => throw new Exception(s"Unexpected match! $other")
  }

  /** Splits an info into non-nested Infos
    * Right now the FIRRTL parser concatenates the infos if using both .fir and .scala source locators, instead of using
    *   MultiInfo. This code will work with both the current concatenation and a future FIRRTL change to migrate to MultiInfo.
    *
    * @param info given fileinfo to flatten
    */
  private [linting] def flatten(info: Info): Seq[FileInfo] = info match {
    case MultiInfo(seq) => seq.flatMap(flatten)
    case f: FileInfo =>
      val infoRegex = "\\s*(.*\\.scala \\d+:\\d+):(.*\\.fir@\\d+\\.\\d+)\\s*".r
      f.info.serialize match {
        case infoRegex(scala, fir) => Seq(FileInfo(StringLit(scala)), FileInfo(StringLit(fir)))
        case other => Seq(f)
      }
    case other => Nil
  }

  /** Returns the first .scala source location contained inside info
    *
    * @param info given fileinfo to find scala fileinfo
    */
  private [linting] def getScalaInfo(info: Info): Option[FileInfo] = flatten(info).collectFirst {
    case i: FileInfo if i.serialize.contains("scala") => i
  }

  /** Returns whether the given file is contained in the whiteList
    *
    * @param info given fileinfo to determine if it is whitelisted
    * @param whiteList list of files to exempt from lint rule
    */
  private [linting] def isWhitelisted(info: Info, whiteList: Set[String]): Boolean = {
    flatten(info).exists { i =>
      val file = i.info.serialize.split(' ').head
      whiteList.contains(file)
    } 
  }

  /** Records a linting violation
    *
    * @param info given fileinfo of the violation
    * @param message Message to include in the violation report
    * @param violations container of existing violations
    * @param mname module name containing the violation
    */
  private [linting] def updateViolations(info: Info, message: String, violations: Violations, mname: String): Unit = {
      val mods = violations.getOrElse((info, message), Set.empty)
      violations((info, message)) = mods + mname
  }

  // Container of violations
  private [linting] type Violations = mutable.HashMap[(Info, String), Set[String]]
}
