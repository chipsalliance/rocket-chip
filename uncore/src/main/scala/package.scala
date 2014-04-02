package object uncore extends uncore.constants.MemoryOpConstants
{
  implicit def toOption[A](a: A) = Option(a)
}
