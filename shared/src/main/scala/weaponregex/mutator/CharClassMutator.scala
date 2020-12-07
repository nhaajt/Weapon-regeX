package weaponregex.mutator

import weaponregex.model.mutation.TokenMutator
import weaponregex.model.regextree._

object CharClassNegation extends TokenMutator {
  override val name = "Character Class Negation"
  override val levels: Seq[Int] = Seq(1)

  override def mutate(token: RegexTree): Seq[String] = (token match {
    case cc: CharacterClass => Seq(cc.copy(isPositive = !cc.isPositive))
    case _                  => Nil
  }) map (_.build)
}

object CharClassRemoveChild extends TokenMutator {
  override val name = "Remove a child from Character Class"
  override val levels: Seq[Int] = Seq(2, 3)

  override def mutate(token: RegexTree): Seq[String] = token match {
    case cc: CharacterClass if cc.children.length > 1 => cc.children map (child => cc.buildWith(child, ""))
    case _                                            => Nil
  }
}

object CharClassAnyChar extends TokenMutator {
  override val name = "Character Class to character class that parses anything"
  override val levels: Seq[Int] = Seq(2, 3)

  override def mutate(token: RegexTree): Seq[String] = (token match {
    case cc: CharacterClass =>
      Seq(
        CharacterClass(Seq(PredefinedCharClass("w", cc.location), PredefinedCharClass("W", cc.location)), cc.location)
      )
    case _ => Nil
  }) map (_.build)
}
