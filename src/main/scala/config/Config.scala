// See LICENSE.SiFive for license details.

package freechips.rocketchip.config

class Field[T]

abstract class View {
  final def apply[T](pname: Field[T]): T = apply(pname, this)
  final def apply[T](pname: Field[T], site: View): T = find(pname, site) match {
    case Some(x) => x.asInstanceOf[T]
  }

  final def lift[T](pname: Field[T]): Option[T] = lift(pname, this)
  final def lift[T](pname: Field[T], site: View): Option[T] = find(pname, site).map(_.asInstanceOf[T])

  protected[config] def find(pname: Any, site: View): Option[Any]
}

abstract class Parameters extends View {
  final def ++ (x: Parameters):                                       Parameters = new ChainParameters(this, x)
  final def alter(f: (View, View, View) => PartialFunction[Any,Any]): Parameters = Parameters(f) ++ this
  final def alterPartial(f: PartialFunction[Any,Any]):                Parameters = Parameters((_,_,_) => f) ++ this

  protected[config] def chain(site: View, tail: View, pname: Any): Option[Any]
  protected[config] def find(pname: Any, site: View) = chain(site, new TerminalView, pname)
}

object Parameters {
  def empty:                                                    Parameters = new EmptyParameters
  def apply(f: (View, View, View) => PartialFunction[Any,Any]): Parameters = new PartialParameters(f)
  def root(p: Parameters) = p
}

class Config(p: Parameters) extends Parameters {
  def this(f: (View, View, View) => PartialFunction[Any,Any]) = this(Parameters(f))

  protected[config] def chain(site: View, tail: View, pname: Any) = p.chain(site, tail, pname)
  override def toString = this.getClass.getSimpleName
  def toInstance = this
}

// Internal implementation:

private class TerminalView extends View {
  def find(pname: Any, site: View): Option[Any] = None
}

private class ChainView(head: Parameters, tail: View) extends View {
  def find(pname: Any, site: View) = head.chain(site, tail, pname)
}

private class ChainParameters(x: Parameters, y: Parameters) extends Parameters {
  def chain(site: View, tail: View, pname: Any) = x.chain(site, new ChainView(y, tail), pname)
}

private class EmptyParameters extends Parameters {
  def chain(site: View, tail: View, pname: Any) = tail.find(pname, site)
}

private class PartialParameters(f: (View, View, View) => PartialFunction[Any,Any]) extends Parameters {
  protected[config] def chain(site: View, tail: View, pname: Any) = {
    val g = f(site, this, tail)
    if (g.isDefinedAt(pname)) Some(g.apply(pname)) else tail.find(pname, site)
  }
}
