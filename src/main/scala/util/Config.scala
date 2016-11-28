// See LICENSE.SiFive for license details.

package config

class Field[T]
class CDEMatchError() extends Exception {
  override def fillInStackTrace() = this
}

abstract class View {
  final def apply[T](pname: Field[T]): T = apply(pname, this)
  final def apply[T](pname: Field[T], site: View): T = find(pname, site).asInstanceOf[T]

  protected[config] def find(pname: Any, site: View): Any
}

abstract class Parameters extends View {
  final def ++ (x: Parameters):                        Parameters = new ChainParameters(this, x)
  final def alter(f: (Any, View, View, View) => Any):  Parameters = Parameters(f) ++ this
  final def alter(m: Map[Any,Any]):                    Parameters = Parameters(m) ++ this
  final def alter(f: PartialFunction[Any,Any]):        Parameters = Parameters(f) ++ this
  final def alterPartial(f: PartialFunction[Any,Any]): Parameters = Parameters(f) ++ this

  protected[config] def chain(site: View, tail: View, pname: Any): Any
  protected[config] def find(pname: Any, site: View) = chain(site, new TerminalView, pname)
}

object Parameters {
  def empty:                                    Parameters = new EmptyParameters
  def apply(f: (Any, View, View, View) => Any): Parameters = new FunctionParameters(f)
  def apply(m: Map[Any,Any]):                   Parameters = new MapParameters(m)
  def apply(f: PartialFunction[Any,Any]):       Parameters = new PartialParameters(f)
  def partial(f: PartialFunction[Any,Any]):     Parameters = new PartialParameters(f)
  def root(p: Parameters) = p
}

class Config(p: Parameters) extends Parameters {
  def this(f: (Any, View, View)       => Any) = this(Parameters((p,s,h,u) => f(p,s,h))) // backwards compat; don't use
  def this(f: (Any, View, View, View) => Any) = this(Parameters(f))
  def this(m: Map[Any,Any])                   = this(Parameters(m))
  def this(f: PartialFunction[Any,Any])       = this(Parameters(f))

  protected[config] def chain(site: View, tail: View, pname: Any) = p.chain(site, tail, pname)
  override def toString = this.getClass.getSimpleName
  def toInstance = this
}

class ConfigPartial(f: PartialFunction[Any,Any]) extends Config(Parameters(f))

// Internal implementation:

private class TerminalView extends View {
  private class Unusable
  def find(pname: Any, site: View): Any = pname match { case x: Unusable => () }
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

private class FunctionParameters(f: (Any, View, View, View) => Any) extends Parameters {
  protected[config] def chain(site: View, tail: View, pname: Any) = {
    try f(pname, site, this, tail)
    catch {
      case e: CDEMatchError    => tail.find(pname, site)
      case e: scala.MatchError => tail.find(pname, site) 
    }
  }
}

private class MapParameters(map: Map[Any, Any]) extends Parameters {
  protected[config] def chain(site: View, tail: View, pname: Any) =
    map.get(pname).getOrElse(find(pname, site))
}

private class PartialParameters(f: PartialFunction[Any,Any]) extends Parameters {
  protected[config] def chain(site: View, tail: View, pname: Any) =
    if (f.isDefinedAt(pname)) f.apply(pname) else tail.find(pname, site)
}
