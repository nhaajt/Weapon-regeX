package weaponregex

import weaponregex.model.mutation._
import weaponregex.model.regextree.RegexTree
import weaponregex.mutator.BuiltinMutators

object TreeMutator {
  implicit class RegexTreeMutator(tree: RegexTree) {
    def mutate(mutationLevel: Int): Seq[Mutant] = mutate(BuiltinMutators(mutationLevel))

    def mutate(mutators: Seq[TokenMutator]): Seq[Mutant] = mutate(mutators, -1)

    def mutate(mutators: Seq[TokenMutator], mutationLevel: Int): Seq[Mutant] =
      (if (mutationLevel == -1) mutators else mutators.filter(_.levels.contains(mutationLevel)))
        .flatMap(mutator =>
          mutator(
            tree,
            mutatedPattern => Mutant(mutatedPattern, MutationData(mutator.name, tree.location, mutator.levels.min))
          )
        ) ++ tree.children.flatMap(child =>
        child.mutate(mutators) map { case mutant @ Mutant(mutatedPattern, _) =>
          mutant.copy(pattern = tree.buildWith(child, mutatedPattern))
        }
      )
  }
}
