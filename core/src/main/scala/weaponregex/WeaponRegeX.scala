package weaponregex

import weaponregex.parser.Parser
import weaponregex.mutator.TreeMutator._
import weaponregex.model.mutation._
import weaponregex.mutator.BuiltinMutators

import scala.scalajs.js.annotation._

/** The API facade of Weapon regeX for Scala/JVM
  */
@JSExportTopLevel("WeaponRegeX")
@JSExportAll
object WeaponRegeX {

  /** Mutate using the given mutators in some specific mutation levels
    * @param pattern Input regex string
    * @param mutators Mutators to be used for mutation
    * @param mutationLevels Target mutation levels. If this is `null`, the `mutators` will not be filtered.
    * @return A sequence of [[weaponregex.model.mutation.Mutant]]
    */
  def mutate(
      pattern: String,
      mutators: Seq[TokenMutator] = BuiltinMutators.all,
      mutationLevels: Seq[Int] = null
  ): Seq[Mutant] =
    Parser(pattern) match {
      case Some(tree) => tree.mutate(mutators, mutationLevels)
      case None       => Nil
    }
}
