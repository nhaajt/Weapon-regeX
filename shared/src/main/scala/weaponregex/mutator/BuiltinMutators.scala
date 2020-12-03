package weaponregex.mutator

import weaponregex.model.mutation.TokenMutator

object BuiltinMutators {
  val all: Seq[TokenMutator] = Seq(
    BOLRemoval,
    EOLRemoval
  )

  lazy val levels: Map[Int, Seq[TokenMutator]] =
    all.foldLeft(Map.empty[Int, Seq[TokenMutator]])((levels, mutator) =>
      mutator.levels.foldLeft(levels)((ls, level) => ls + (level -> (ls.getOrElse(level, Nil) :+ mutator)))
    )

  final def apply(mutationLevel: Int): Seq[TokenMutator] = level(mutationLevel)

  def level(mutationLevel: Int): Seq[TokenMutator] = levels.getOrElse(mutationLevel, Nil)
}
