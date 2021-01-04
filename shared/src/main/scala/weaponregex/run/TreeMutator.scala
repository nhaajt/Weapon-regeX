package weaponregex.run

import weaponregex.model.mutation._
import weaponregex.model.regextree.RegexTree
import weaponregex.mutator.BuiltinMutators

object TreeMutator {
  implicit class RegexTreeMutator(tree: RegexTree) {
    private def filterMutators(mutators: Seq[TokenMutator], mutationLevels: Seq[Int]): Seq[TokenMutator] =
      mutators.filter(mutator => mutationLevels.exists(mutator.levels.contains(_)))

    /** Mutate using the given mutators in some specific mutation levels
      *
      * @param mutators Mutators to be used for mutation
      * @param mutationLevels Target mutation levels. If this is `null`, the `mutators` will not be filtered.
      * @return A sequence of [[weaponregex.model.mutation.Mutant]]
      */
    def mutate(mutators: Seq[TokenMutator] = BuiltinMutators.all, mutationLevels: Seq[Int] = null): Seq[Mutant] = {
      val mutatorsFiltered: Seq[TokenMutator] =
        if (mutationLevels == null) mutators
        else filterMutators(mutators, mutationLevels)

      val rootMutants: Seq[Mutant] = mutatorsFiltered flatMap (mutator =>
        mutator(tree) map (mutatedPattern =>
          Mutant(mutatedPattern, MutationData(mutator.name, tree.location, mutator.levels.min, mutator.description))
        )
      )

      val childrenMutants: Seq[Mutant] = tree.children flatMap (child =>
        child.mutate(mutatorsFiltered) map { case mutant @ Mutant(mutatedPattern, _) =>
          mutant.copy(pattern = tree.buildWith(child, mutatedPattern))
        }
      )

      rootMutants ++ childrenMutants
    }
  }
}
