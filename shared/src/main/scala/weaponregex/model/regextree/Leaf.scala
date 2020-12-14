package weaponregex.model.regextree

import weaponregex.model.Location

// Why are prefix and postfix implicit? Especially since they are both Strings it is very easy to accidentally pass the wrong parameter
// The double constructor parantheses are also a little unusual
// If this is so you can create classes while only passing a single parameter I would suggest something like this:
/** ```
  * abstract class Leaf[A](location: Location) extends RegexTree {
  *   val value: A
  * }
  *
  * case class BOL(location: Location) extends Leaf(location) {
  *   val value = '^'
  * }
  * ```
  */

abstract class Leaf[A](val value: A)(override val location: Location)(implicit
    override val prefix: String = "",
    override val postfix: String = ""
) extends RegexTree {
  override val children: Seq[RegexTree] = Nil
  override protected def _build: String = prefix + value + postfix

  override def buildWith(child: RegexTree, childString: String): String = build
}

case class Character(char: Char, override val location: Location) extends Leaf(char)(location)

// Any is already a type in Scala (it is like Object). Please rename this to prevent any clashes
// For example, pattern matching on Any would make it unclear if you are matching on this or the scala Any and is quite error-prone
// If it is a predefined character class without any negations, couldn't that case be handled in PredefinedCharacterClass instead of making a separate case class for it?
// "Any" is technically a predefined character class, but because it cannot be negated, it is handled separately
case class Any(override val location: Location) extends Leaf('.')(location)

/** @param metaChar Can be any meta character as defined in the grammar
  * @param location [[weaponregex.model.Location]] of the token in the regex string
  */
case class MetaChar(metaChar: String, override val location: Location) extends Leaf(metaChar)(location)("""\""")

case class PredefinedCharClass(charClass: String, override val location: Location)
    extends Leaf(charClass)(location)("""\""")

// Overall this syntax of creating trees is great and very clean though!
case class BOL(override val location: Location) extends Leaf('^')(location)

case class EOL(override val location: Location) extends Leaf('$')(location)

/** Boundary meta character
  *
  * @param boundary Can be any boundary character as defined in the grammar
  * @param location Location of the token in the regex string
  */
case class Boundary(boundary: String, override val location: Location) extends Leaf(boundary)(location)

case class NameReference(name: String, override val location: Location) extends Leaf(name)(location)("""\k<""", ">")

case class NumberReference(num: Int, override val location: Location) extends Leaf(num)(location)("""\""")

case class QuoteChar(char: Char, override val location: Location) extends Leaf(char)(location)("""\""")

case class Quote(quote: String, hasEnd: Boolean, override val location: Location)
    extends Leaf(quote)(location)("""\Q""", if (hasEnd) """\E""" else "")
