package object uncore extends
  uncore.constants.MemoryOpConstants with
  uncore.constants.TileLinkSizeConstants with
  uncore.constants.MemoryInterfaceConstants
{
  implicit def toOption[A](a: A) = Option(a)
}
