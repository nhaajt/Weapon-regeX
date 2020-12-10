package weaponregex.mutator

import weaponregex.model.mutation.TokenMutator
import weaponregex.model.regextree._

object BOLRemoval extends TokenMutator {
  override val name: String = "Beginning of line character `^` removal"
  override val levels: Seq[Int] = Seq(1, 2, 3)
  override val description: String = "Remove beginning of line character `^`"

  override def mutate(token: RegexTree): Seq[String] = token.children.foldLeft(Seq.empty[String])((results, child) =>
    child match {
      case _: BOL => results :+ token.buildWith(child, "")
      case _      => results
    }
  )
}

object EOLRemoval extends TokenMutator {
  override val name: String = "End of line character `$` removal"
  override val levels: Seq[Int] = Seq(1, 2, 3)
  override val description: String = "Remove end of line character `$`"

  override def mutate(token: RegexTree): Seq[String] = token.children.foldLeft(Seq.empty[String])((results, child) =>
    child match {
      case _: EOL => results :+ token.buildWith(child, "")
      case _      => results
    }
  )
}
