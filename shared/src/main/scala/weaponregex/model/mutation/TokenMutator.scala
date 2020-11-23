package weaponregex.model.mutation

import weaponregex.model.regextree.RegexTree

trait TokenMutator {
  val name: String
  val levels: Seq[Int]
  val description: String = name

  def apply(tree: RegexTree): Seq[String]

  def apply[A](tree: RegexTree, transformer: String => A): Seq[A] =
    apply(tree) map transformer

  override def toString: String = name
}
