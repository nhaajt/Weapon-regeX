package weaponregex.model.mutation

import weaponregex.model.regextree.RegexTree

trait TokenMutator {
  val name: String
  val levels: Seq[Int]
  val description: String = name

  final def apply(token: RegexTree): Seq[String] = mutate(token)

  /** I'm noticing most of the implementations for this function have a pattern match on the expected tree type of the mutator.
    * Perhaps the mutate function could be generalized so a mutator only accepts the expected Tree type?
    * I would also expect this to return a `Seq[RegexTree]`
    */

  def mutate(token: RegexTree): Seq[String]

  // Why override toString?
  override def toString: String = name
}
