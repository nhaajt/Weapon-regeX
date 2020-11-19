package weaponregex

import weaponregex.model.mutation._
import weaponregex.model.regextree.RegexTree

object TreeMutator {
  implicit class RegexTreeMutator(tree: RegexTree) {
    def mutate(mutators: Seq[TokenMutator]): Seq[Mutant] = {
      mutators
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
}
