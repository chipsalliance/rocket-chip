package ReferenceChip

import sys.process._
import Chisel._

object Main
{
  def classOf(x: String) = try {
    Class.forName(x)
  } catch {
    case _: ClassNotFoundException => {
      val myPackage = getClass.getName.reverse.dropWhile(_ != '.').reverse
      Class.forName(myPackage+x)
    }
  }

  def main(allArgs: Array[String]): Unit = {
    require(allArgs.length >= 4, "syntax: run <command> <component> <backend> <dir> [args]")
    val cmd = allArgs(0)
    val compName = allArgs(1)
    val backendName = allArgs(2)
    val dir = allArgs(3)

    val backend = try {
      classOf(backendName).getName
    } catch {
      case _ => backendName
    }

    println("I'm gonna "+cmd+" component "+compName+" with backend "+backendName+"...")
    val comp = classOf(compName)

    val chiselArgs = allArgs.drop(4) ++ Array("--targetDir", dir, "--backend", backend)

    val makeDir = dir+"/.."
    val jobs = Runtime.getRuntime.availableProcessors
    val make = "make -C" + makeDir + " -j" + jobs
    val action = cmd match {
      case "elaborate" => () =>
      case "build" => () => make!
      case "test" => () => {
        if ((make+" run-fast"!) == 0) println("PASSED")
        else println("FAILED")
      }
      case _ => throw new IllegalArgumentException("unknown command "+cmd)
    }

    chiselMain(chiselArgs, () => comp.newInstance.asInstanceOf[Component])
    action()
  }
}
