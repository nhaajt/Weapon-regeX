package weaponregex

import weaponregex.model.mutation._
import weaponregex.model.regextree.RegexTree
import weaponregex.mutator.BuiltinMutators

object TreeMutator {
  implicit class RegexTreeMutator(tree: RegexTree) {

    /** Mutate using all the built-in mutators in a specific mutation level
      * @param mutationLevel Target mutation level
      * @return A sequence of [[Mutant]]
      */
    def mutate(mutationLevel: Int): Seq[Mutant] = mutate(BuiltinMutators(mutationLevel))

    /** Mutate using the given mutators in a specific mutation level
      * @param mutators Mutators to be used for mutation
      * @param mutationLevel Target mutation level
      * @return A sequence of [[Mutant]]
      */
    def mutate(mutators: Seq[TokenMutator], mutationLevel: Int): Seq[Mutant] =
      mutate(mutators.filter(_.levels.contains(mutationLevel)))

    /** Mutate using the given mutators in all mutation levels
      * @param mutators Mutators to be used for mutation
      * @return A sequence of [[Mutant]]
      */
    def mutate(mutators: Seq[TokenMutator]): Seq[Mutant] =
      mutators.flatMap(mutator =>
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
