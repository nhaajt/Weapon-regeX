package WeaponRegeX.model.RegexTree

import WeaponRegeX.model.Location

abstract class Leaf(val value: String)(override val location: Location) extends RegexTree {
  def this(value: AnyVal)(location: Location) = this(value.toString)(location)

  override val children: Seq[RegexTree] = Nil
  def build: String = value
}

case class Character(char: Char, override val location: Location) extends Leaf(char.toString)(location)

// "Any" is technically a predefined character class, but because it cannot be negated, it is handled separately
case class Any(override val location: Location) extends Leaf('.')(location)

/** @param metaChar Can be any meta character as defined in the grammar
  * @param location Location of the token in the regex string
  */
case class MetaChar(metaChar: String, override val location: Location) extends Leaf(metaChar)(location)

case class PredefinedCharClass(charClass: String, isPositive: Boolean, override val location: Location)
    extends Leaf(charClass)(location) {
  override def build: String = "\\" + (if (isPositive) charClass.toLowerCase else charClass.toUpperCase)
}

/** A wrapper for boundary meta character
  *
  * @param boundary Can be any boundary character as defined in the grammar
  * @param location Location of the token in the regex string
  */
case class Boundary(boundary: String, override val location: Location) extends Leaf(boundary)(location)

case class FlagToggle(onFlags: String, hasDash: Boolean, offFlags: String, override val location: Location)
    extends Leaf(s"(?$onFlags${if (hasDash) "-" else ""}$offFlags)")(location)

case class NameReference(name: String, override val location: Location) extends Leaf(s"\\k<$name>")(location)

case class NumberReference(num: Int, override val location: Location) extends Leaf(s"\\$num")(location)

case class QuoteChar(char: Char, override val location: Location) extends Leaf(s"\\$char")(location)

case class Quote(quote: String, hasEnd: Boolean, override val location: Location)
    extends Leaf(s"\\Q$quote${if (hasEnd) "\\E" else ""}")(location)
