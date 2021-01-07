package weaponregex.mutator

import weaponregex.model.mutation.TokenMutator
import weaponregex.model.regextree._

object GroupToNCGroup extends TokenMutator {
  override val name: String = "Capturing group to non-capturing group"
  override val levels: Seq[Int] = Seq(2, 3)
  override val description: String = "Capturing group to non-capturing group"

  override def mutate(token: RegexTree): Seq[String] = (token match {
    case cc @ Group(_, true, _) => Seq(cc.copy(isCapturing = false))
    case _                      => Nil
  }) map (_.build)
}
