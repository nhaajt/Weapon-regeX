package weaponregex.mutator

import weaponregex.model.mutation.TokenMutator
import weaponregex.model.regextree._

object BOLRemoval extends TokenMutator {
  override val name: String = "Beginning of line character (^) removal"
  override val levels: Seq[Int] = Seq(1, 2, 3)

  override def apply(tree: RegexTree): Seq[String] = tree.children.foldLeft(Seq.empty[String])((results, child) =>
    child match {
      case _: BOL => results :+ tree.buildWith(child, "")
      case _      => results
    }
  )
}
