package weaponregex.mutator

import weaponregex.model.mutation.TokenMutator
import scala.scalajs.js.annotation._

/** Built-in token mutators
  */
@JSExportTopLevel("BuiltinMutators")
@JSExportAll
object BuiltinMutators {

  /** Sequence of all built-in token mutators
    */
  val all: Seq[TokenMutator] = Seq(
    BOLRemoval,
    EOLRemoval,
    BOL2BOI,
    EOL2EOI,
    CharClassNegation,
    CharClassChildRemoval,
    CharClassAnyChar,
    CharClassRangeModification,
    PredefCharClassNegation,
    PredefCharClassNullification,
    PredefCharClassAnyChar,
    QuantifierRemoval,
    QuantifierNChange,
    QuantifierNOrMoreModification,
    QuantifierNOrMoreChange,
    QuantifierNMModification,
    QuantifierShortModification,
    QuantifierShortChange,
    QuantifierReluctantAddition,
    GroupToNCGroup
  )

  /** Map from mutation level number to token mutators in that level
    */
  lazy val levels: Map[Int, Seq[TokenMutator]] =
    all.foldLeft(Map.empty[Int, Seq[TokenMutator]])((levels, mutator) =>
      mutator.levels.foldLeft(levels)((ls, level) => ls + (level -> (ls.getOrElse(level, Nil) :+ mutator)))
    )

  final def apply(mutationLevel: Int): Seq[TokenMutator] = level(mutationLevel)

  /** Get all the tokens mutators given a mutation level number
    * @param mutationLevel Mutation level number
    * @return Sequence of all the tokens mutators in that level, if any
    */
  def level(mutationLevel: Int): Seq[TokenMutator] = levels.getOrElse(mutationLevel, Nil)
}
