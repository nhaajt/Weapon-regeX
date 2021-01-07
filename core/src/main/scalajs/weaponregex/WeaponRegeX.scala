package weaponregex

import weaponregex.parser.Parser
import weaponregex.mutator.TreeMutator._
import weaponregex.model.mutation._
import weaponregex.mutator.BuiltinMutators

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation._

/** Main facade of Weapon regeX
  */
object WeaponRegeX {

  /** JavaScript Dictionary that map from token mutator class names to the associating token mutators
    */
  @JSExportTopLevel("mutators")
  val allMutators: js.Map[String, TokenMutator] =
    BuiltinMutators.all
      .map(mutator => mutator.getClass.getSimpleName.split("\\$$").head -> mutator)
      .toMap
      .toJSMap

  class MutationOptions(
      val mutators: js.Array[TokenMutator] = BuiltinMutators.all.toJSArray,
      val mutationLevels: js.Array[Int] = null
  ) extends js.Object

  /** Mutate using the given mutators in some specific mutation levels
    *
    * @param pattern Input regex string
    * @param options Mutation options
    * {{{
    * {
    *   mutators: [Mutators to be used for mutation],
    *   mutationLevels: [Target mutation levels. If this is `null`, the `mutators` will not be filtered],
    * }
    * }}}
    * @return A JavaScript Array of [[weaponregex.model.mutation.Mutant]]
    */
  @JSExportTopLevel("mutate")
  def mutate(pattern: String, options: MutationOptions = new MutationOptions()): js.Array[Mutant] = {
    val mutators =
      if (options.hasOwnProperty("mutators") && options.mutators != null) options.mutators.toSeq
      else BuiltinMutators.all
    val mutationLevels =
      if (options.hasOwnProperty("mutationLevels") && options.mutationLevels != null) options.mutationLevels.toSeq
      else null

    (Parser(pattern) match {
      case Some(tree) => tree.mutate(mutators, mutationLevels)
      case None       => Nil
    }).toJSArray
  }
}
