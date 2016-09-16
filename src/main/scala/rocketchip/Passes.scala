// See LICENSE for license details.

package rocketchip

import firrtl._
import firrtl.ir._
import firrtl.passes.Pass

// Converts some modules to external modules, based on a given function.  If
// that function returns "true" then the module is converted into an ExtModule,
// otherwise it's left alone.
class ConvertToExtMod(classify: (Module) => Boolean) extends Pass {
  def name = "Convert to External Modules"

  def run(c: Circuit): Circuit = {
    val modulesx = c.modules.map {
      case m: ExtModule => m
      case m: Module =>
        if (classify(m)) {
          new ExtModule(m.info, m.name, m.ports)
        } else {
          m
        }
    }
    Circuit(c.info, modulesx, c.main)
  }
}

// "Re-Parents" a circuit, which changes the top module to something else.
class ReParentCircuit(newTopName: String) extends Pass {
  def name = "Re-Parent Circuit"

  def run(c: Circuit): Circuit = {
    Circuit(c.info, c.modules, newTopName)
  }
}

// Removes all the unused modules in a circuit by recursing through every
// instance (starting at the main module)
class RemoveUnusedModules extends Pass {
  def name = "Remove Unused Modules"

  def run(c: Circuit): Circuit = {
    val modulesByName = c.modules.map{
      case m: Module => (m.name, Some(m))
      case m: ExtModule => (m.name, None)
    }.toMap

    def getUsedModules(om: Option[Module]): Set[String] = {
      om match {
        case Some(m) => {
          def someStatements(statement: Statement): Seq[Statement] =
            statement match {
              case b: Block => 
                b.stmts.map{ someStatements(_) }
                  .foldLeft(Seq[Statement]())(_ ++ _)
              case i: DefInstance => Seq(i)
              case w: WDefInstance => Seq(w)
              case _ => Seq()
            }

            someStatements(m.body).map{
              case s: DefInstance => Set(s.module) | getUsedModules(modulesByName(s.module))
              case s: WDefInstance => Set(s.module) | getUsedModules(modulesByName(s.module))
              case _ => Set[String]()
            }.foldLeft(Set(m.name))(_ | _)
          }

        case None => Set.empty[String]
      }
    }
    val usedModuleSet = getUsedModules(modulesByName(c.main))

    val usedModuleSeq = c.modules.filter { usedModuleSet contains _.name }

    Circuit(c.info, usedModuleSeq, c.main)
  }
}
