package weaponregex.mutator

import weaponregex.model.mutation.TokenMutator
import weaponregex.model.regextree._
import weaponregex.`extension`.StringExtension._

object PredefCharClassNegation extends TokenMutator {
  override val name = "Predefined Character Class Negation"
  override val levels: Seq[Int] = Seq(1)

  override def mutate(token: RegexTree): Seq[String] = (token match {
    case pdcc: PredefinedCharClass => Seq(pdcc.copy(charClass = pdcc.charClass.toggleCase))
    case _                         => Nil
  }) map (_.build)
}

object PredefCharClassNullification extends TokenMutator {
  override val name = "Predefined Character Class Nullification"
  override val levels: Seq[Int] = Seq(2, 3)
  override val description: String = "Nullify a predefined character class by removing the `\\`"

  override def mutate(token: RegexTree): Seq[String] = token match {
    case pdcc: PredefinedCharClass => Seq(pdcc.charClass)
    case _                         => Nil
  }
}

object PredefCharClassAnyChar extends TokenMutator {
  override val name = "Predefined Character Class to character class with its negation"
  override val levels: Seq[Int] = Seq(2, 3)
  override val description: String = "Adding the negation of that predefined character class to match any character"

  override def mutate(token: RegexTree): Seq[String] = (token match {
    case pdcc: PredefinedCharClass =>
      Seq(CharacterClass(Seq(pdcc, pdcc.copy(charClass = pdcc.charClass.toggleCase)), pdcc.location))
    case _ => Nil
  }) map (_.build)
}
