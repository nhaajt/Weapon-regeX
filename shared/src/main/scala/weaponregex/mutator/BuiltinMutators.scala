package weaponregex.mutator

import weaponregex.model.mutation.TokenMutator

object BuiltinMutators {
  val all: Seq[TokenMutator] = Seq(
    BOLRemoval,
    EOLRemoval,
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
    QuantifierReluctantAddition
  )

  lazy val levels: Map[Int, Seq[TokenMutator]] =
    all.foldLeft(Map.empty[Int, Seq[TokenMutator]])((levels, mutator) =>
      mutator.levels.foldLeft(levels)((ls, level) => ls + (level -> (ls.getOrElse(level, Nil) :+ mutator)))
    )

  final def apply(mutationLevel: Int): Seq[TokenMutator] = level(mutationLevel)

  def level(mutationLevel: Int): Seq[TokenMutator] = levels.getOrElse(mutationLevel, Nil)
}
