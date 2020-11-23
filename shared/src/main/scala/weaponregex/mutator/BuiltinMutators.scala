package weaponregex.mutator

import weaponregex.model.mutation.TokenMutator

object BuiltinMutators {
  val builtinMutators: Seq[TokenMutator] = Seq(
    BOLRemoval,
    EOLRemoval
  )

  lazy val levels: Map[Int, Seq[TokenMutator]] =
    builtinMutators.foldLeft(Map.empty[Int, Seq[TokenMutator]])((levels, mutator) =>
      mutator.levels.foldLeft(levels)((ls, level) => ls + (level -> (ls.getOrElse(level, Nil) :+ mutator)))
    )

  def apply(mutationLevel: Int): Seq[TokenMutator] = level(mutationLevel)

  def get: Seq[TokenMutator] = builtinMutators

  def level(mutationLevel: Int): Seq[TokenMutator] = levels.getOrElse(mutationLevel, Nil)
}
