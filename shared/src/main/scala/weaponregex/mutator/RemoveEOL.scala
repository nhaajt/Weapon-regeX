package weaponregex.mutator

import weaponregex.model.mutation.TokenMutator
import weaponregex.model.regextree._

object RemoveEOL extends TokenMutator {
  override val name: String = "Remove end of line character ($)"
  override val levels: Seq[Int] = Seq(1, 2, 3)

  override def apply(tree: RegexTree): Seq[String] = tree.children.foldLeft(Seq.empty[String])((results, child) =>
    child match {
      case _: EOL => results :+ tree.buildWith(child, "")
      case _      => results
    }
  )
}
