package weaponregex

import weaponregex.model.regextree._
import weaponregex.mutator.BuiltinMutators
import weaponregex.model.mutation._
import weaponregex.model._
import weaponregex.run.Parser

import scala.scalajs.js.annotation._
import weaponregex.run.TreeMutator

/** Main facade of Weapon regeX
  */
object WeaponRegeX {

  def mutate(pattern: String, mutationLevel: Int): Seq[Mutant] = {
    val parsedTree = Parser.parseOrError(pattern)
    return TreeMutator.RegexTreeMutator(parsedTree).mutate(mutationLevel)
  }

  def mutate(pattern: String, mutators: Seq[TokenMutator], mutationLevel: Int): Seq[Mutant] = {
    val parsedTree = Parser.parseOrError(pattern)
    return TreeMutator.RegexTreeMutator(parsedTree).mutate(mutators, mutationLevel)
  }

  def mutate(pattern: String, mutators: Seq[TokenMutator]): Seq[Mutant] = {
    val parsedTree = Parser.parseOrError(pattern)
    return TreeMutator.RegexTreeMutator(parsedTree).mutate(mutators)
  }
}
