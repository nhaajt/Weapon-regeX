package weaponregex

import weaponregex.run.Parser
import weaponregex.run.TreeMutator._
import weaponregex.model.mutation._

import scala.scalajs.js.annotation._

// class WRegex(var pattern: String, var flags: String = "")

/** Main facade of Weapon regeX
  */
@JSExportTopLevel("WeaponRegeX")
@JSExportAll
object WeaponRegeX {

  def mutate(pattern: String): Seq[Mutant] =
    Parser(pattern) match {
      case Some(tree) => tree.mutate
      case None       => Nil
    }

  def mutate(pattern: String, mutationLevel: Int): Seq[Mutant] =
    Parser(pattern) match {
      case Some(tree) => tree.mutate(mutationLevel)
      case None       => Nil
    }

  def mutate(pattern: String, mutators: Seq[TokenMutator], mutationLevel: Int): Seq[Mutant] =
    Parser(pattern) match {
      case Some(tree) => tree.mutate(mutators, mutationLevel)
      case None       => Nil
    }

  def mutate(pattern: String, mutators: Seq[TokenMutator]): Seq[Mutant] =
    Parser(pattern) match {
      case Some(tree) => tree.mutate(mutators)
      case None       => Nil
    }
}
