package weaponregex

import weaponregex.model.regextree._
import weaponregex.mutator.BuiltinMutators
import weaponregex.model.mutation._
import weaponregex.model._
import weaponregex.run.Parser

import scala.scalajs.js.annotation._
import weaponregex.run.TreeMutator

// class WRegex(var pattern: String, var flags: String = "")

/** Main facade of Weapon regeX
  */
@JSExportTopLevel("WeaponRegeX")
@JSExportAll
object WeaponRegeX {

  def mutate(regex: String, mutationLevel: Int): Seq[Mutant] = {
    val parsedTree = Parser.parseOrError(regex)
    return TreeMutator.RegexTreeMutator(parsedTree).mutate(mutationLevel)
  }

  def mutate(regex: String, mutators: Seq[TokenMutator], mutationLevel: Int): Seq[Mutant] = {
    val parsedTree = Parser.parseOrError(regex)
    return TreeMutator.RegexTreeMutator(parsedTree).mutate(mutators, mutationLevel)
  }

  def mutate(regex: String, mutators: Seq[TokenMutator]): Seq[Mutant] = {
    val parsedTree = Parser.parseOrError(regex)
    return TreeMutator.RegexTreeMutator(parsedTree).mutate(mutators)
  }
}
