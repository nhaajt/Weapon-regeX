package weaponregex

import weaponregex.run.Parser
import weaponregex.run.TreeMutator._
import weaponregex.model.mutation._

import scala.scalajs.js.annotation._

/** Main facade of Weapon regeX
  */
@JSExportTopLevel("WeaponRegeX")
object WeaponRegeX {

  @JSExport
  def mutate(pattern: String): Seq[Mutant] =
    Parser(pattern) match {
      case Some(tree) => tree.mutate
      case None       => Nil
    }

  @JSExport
  def mutate(pattern: String, mutationLevel: Int): Seq[Mutant] =
    Parser(pattern) match {
      case Some(tree) => tree.mutate(mutationLevel)
      case None       => Nil
    }

  @JSExport
  def mutate(pattern: String, mutators: Seq[TokenMutator], mutationLevel: Int): Seq[Mutant] =
    Parser(pattern) match {
      case Some(tree) => tree.mutate(mutators, mutationLevel)
      case None       => Nil
    }

  @JSExport
  def mutate(pattern: String, mutators: Seq[TokenMutator]): Seq[Mutant] =
    Parser(pattern) match {
      case Some(tree) => tree.mutate(mutators)
      case None       => Nil
    }
}
