package weaponregex.model.mutation

import weaponregex.model.regextree.RegexTree

trait TokenMutator {
  val name: String
  val levels: Seq[Int]
  val description: String = name

  final def apply(token: RegexTree): Seq[String] = mutate(token)

  def mutate(token: RegexTree): Seq[String]

  override def toString: String = name
}
