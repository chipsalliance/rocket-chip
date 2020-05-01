// See LICENSE.SiFive for license details.

package freechips.rocketchip.linting

import firrtl._
import firrtl.ir._
import scala.collection.mutable

package object rule {
  /** Determines whether name is prepended with an underscore, indicating a bad name */
  private [linting] def isTemporary(name: String): Boolean = name.nonEmpty && name.head == '_'

  /** Determines whether name is prepended with an underscore, indicating a bad name */
  private [linting] def isTemporary(expr: Expression): Boolean = isTemporary(getName(expr))

  /** Returns the root reference name of an Expression
    * @throws Exception
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

  /** Returns the first .scala source location contained inside info */
  private [linting] def getScalaInfo(info: Info): Option[FileInfo] = flatten(info).collectFirst {
    case i: FileInfo if i.serialize.contains("scala") => i
  }

  /** Returns whether the given file is contained in the whiteList */
  private [linting] def isWhitelisted(info: Info, whiteList: Set[String]): Boolean = {
    flatten(info).exists { i =>
      val file = i.info.serialize.split(' ').head
      whiteList.contains(file)
    } 
  }

  private [linting] def updateErrors(info: Info, message: String, errors: Errors, mname: String): Unit = {
      val mods = errors.getOrElse((info, message), Set.empty)
      errors((info, message)) = mods + mname
  }

  private [linting] type Errors = mutable.HashMap[(Info, String), Set[String]]

}
