// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import firrtl.RenameMap
import firrtl.annotations.{Annotation, HasSerializationHints}
import firrtl.annotations.{IsModule, ReferenceTarget}

import chisel3.{Data, SyncReadMem}
import chisel3.experimental.{BaseModule, ChiselAnnotation}

import scala.collection.mutable

/** Like [[ElaborationArtefact]] but in annotation form.
  *
  * Does not implement [[CustomFileEmission]] because outputFile should be
  * interpreted as the final, static filepath instead of computing from
  * [[StageOptions]]. This is because the full outputFile path is computed by
  * chisel but the contents are emitted by firrtl, which may have a different
  * set of [[StageOptions]].
  *
  * FIXME: tokens should be [[List[Token]]] but JSON serialization fails with
  *        "no usable constructor for Token"
  */
case class ElaborationArtefactAnnotation(outputFile: String, tokens: List[Any]) extends Annotation with HasSerializationHints {
  def update(renames: RenameMap): Seq[Annotation] = {
    Seq(this.copy(tokens = tokens.collect {
      case t: Token => t.update(renames)
      case other => Seq(other)
    }.flatten))
  }

  def typeHints: Seq[Class[_]] = Seq(
    classOf[Token],
    classOf[StringToken],
    classOf[ModulePathToken],
    classOf[ReferencePathToken],
  )
}

object ElaborationArtefactAnnotation {
  /** Emits [[ElaborationArtefactAnnotation]] for the given filename extension and tokens.
    */
  def annotate(filename: String, tokens: => Seq[Token]): Unit = {
    chisel3.experimental.annotate(new ChiselAnnotation {
      def toFirrtl = ElaborationArtefactAnnotation(filename, tokens.toList)
    })
  }
}


sealed trait Token {
  def update(renames: RenameMap): Seq[Token]
}

case class StringToken(value: String) extends Token {
  def update(renames: RenameMap) = Seq(this)
}

case class ModulePathToken(target: IsModule) extends Token {
  def update(renames: RenameMap) = {
    renames.get(target) match {
      case None => Seq(this)
      case Some(Seq(newModule: IsModule)) => Seq(this.copy(target = newModule))
      case Some(other) => throw new Exception(s"module $target cannot be renamed to $other")
    }
  }
}

case class MemoryPathToken(target: ReferenceTarget) extends Token {
  def update(renames: RenameMap) = {
    renames.get(target) match {
      case None => Seq(this)
      case Some(Seq()) => throw new Exception(s"memory $target was deleted")
      case Some(Seq(one: ReferenceTarget)) => Seq(this.copy(target = one))
      case Some(many) =>
        many.tail.foldLeft(Seq[Token](MemoryPathToken(many.head.asInstanceOf[ReferenceTarget]))) {
          case (tokens, r: ReferenceTarget) => this.copy(target = r) +: StringToken(" ") +: tokens
        }.reverse
    }
  }
}

case class ReferencePathToken(target: ReferenceTarget) extends Token {
  def update(renames: RenameMap) = {
    renames.get(target) match {
      case None => Seq(this)
      case Some(Seq(newRef: ReferenceTarget)) => Seq(this.copy(target = newRef))
      case Some(other) => throw new Exception(s"reference $target cannot be renamed to $other")
    }
  }
}

object Token {
  /** An interpolator that generates tokens. Arguments for which a
    * [[Tokenizer]] instance is defined will be turned into a [[Token]] using
    * the [[Tokenizer]] instance, [[Seq]] elements will be recursively
    * converted to Tokens, and all other values will be turned into
    * [[StringToken]]s using their .toString method.
    */
  implicit class TokensInterpolator(private val sc: StringContext) extends AnyVal {
    def tokens(args: Any*): Seq[Token] = {
      val strings = sc.parts.map(StringContext.treatEscapes).iterator
      val expressions = args.iterator
      var tokenBuf = new mutable.ArrayBuffer[Token]()
      // buffer to build up the next string token
      val stringBuf = new StringBuilder(strings.next())
      def append(any: Any): Unit = {
        var nonStringToken: Option[Token] = None
        any match {
          case s: String => stringBuf ++= s
          case d: Data => nonStringToken = Some(Token(d))
          case m: SyncReadMem[_] => nonStringToken= Some(Token(m))
          case m: BaseModule => nonStringToken = Some(Token(m))
          case t: Token => nonStringToken = Some(t)
          case seq: Seq[_] => seq.foreach(append)
          case other => stringBuf ++= other.toString
        }
        if (nonStringToken.isDefined) {
          if (stringBuf.nonEmpty) {
            tokenBuf += StringToken(stringBuf.toString)
            stringBuf.clear()
          }
          tokenBuf += nonStringToken.get
        }
      }
      while (strings.hasNext) {
        append(expressions.next())
        stringBuf ++= strings.next()
      }
      tokenBuf += StringToken(stringBuf.toString)
      tokenBuf.toSeq
    }
  }

  def apply[T: Tokenizer](t: T): Token = Tokenizer[T].toToken(t)
}


sealed trait Tokenizer[T] {
  def toToken(t: T): Token
}

object Tokenizer {
  def apply[T: Tokenizer] = implicitly[Tokenizer[T]]

  private def tokenizer[T](fn: T => Token): Tokenizer[T] = new Tokenizer[T] {
    def toToken(t: T) = fn(t)
  }

  implicit def stringTokenizer: Tokenizer[String] = tokenizer(StringToken(_: String))
  implicit def modulePathTokenizer: Tokenizer[BaseModule] = tokenizer((m: BaseModule) => ModulePathToken(m.toTarget))
  implicit def memPathTokenizer[T <: Data]: Tokenizer[SyncReadMem[T]] = tokenizer((m: SyncReadMem[_]) => MemoryPathToken(m.toTarget))
  implicit def refPathTokenizer[T <: Data]: Tokenizer[T] = tokenizer((d: T) => ReferencePathToken(d.toTarget))
}
