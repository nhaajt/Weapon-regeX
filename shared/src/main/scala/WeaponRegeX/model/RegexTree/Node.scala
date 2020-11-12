package WeaponRegeX.model.RegexTree
import WeaponRegeX.model.Location

abstract class Node(override val children: RegexTree*)(override val location: Location)(implicit
    val prefix: String = "",
    val postfix: String = "",
    val sep: String = ""
) extends RegexTree {
  override def build: String = prefix + children.map(_.build).mkString(sep = sep) + postfix
}

case class CharacterClass(nodes: Seq[RegexTree], override val location: Location, isPositive: Boolean = true)
    extends Node(nodes: _*)(location)(prefix = if (isPositive) "[" else "[^", postfix = "]") {}

case class Range(from: Character, to: Character, override val location: Location)
    extends Node(from, to)(location)(sep = "-")

case class Group(
    expr: RegexTree,
    isCapturing: Boolean,
    onFlags: String,
    hasDash: Boolean,
    offFlags: String,
    override val location: Location
) extends Node(expr)(location)(if (isCapturing) "(" else s"(?$onFlags${if (hasDash) "-" else ""}$offFlags:", ")")

case class NamedGroup(expr: RegexTree, name: String, override val location: Location)
    extends Node(expr)(location)(s"(?<$name>", ")")

case class Lookaround(expr: RegexTree, isPositive: Boolean, isLookahead: Boolean, override val location: Location)
    extends Node(expr)(location)(
      s"(?${if (isLookahead) "" else "<"}${if (isPositive) "=" else "!"}",
      ")"
    )

// Independent non-capturing group
case class INCGroup(expr: RegexTree, override val location: Location) extends Node(expr)(location)("(?>", "")

// Infinity will be represented as -1
case class Quantifier(
    expr: RegexTree,
    min: Int,
    hasComma: Boolean,
    max: Int,
    override val location: Location,
    isReluctant: Boolean = false,
    isPossessive: Boolean = false
) extends Node(expr)(location)(
      postfix = s"{$min${if (hasComma) "," else ""}$max}${if (isReluctant) "?" else if (isPossessive) "+" else ""}"
    )
// multiline syntax alternative:
//    extends Node(expr)(location)(postfix = {
//  var quantifier = s"{$min${if (hasComma) "," else ""}$max}"
//  if (isReluctant) quantifier += "?"
//  if (isPossessive) quantifier += "+"
//  quantifier
//})

case class ZeroOrOne(
    expr: RegexTree,
    override val location: Location,
    isReluctant: Boolean = false,
    isPossessive: Boolean = false
) extends Node(expr)(location)(postfix = s"?${if (isReluctant) "?" else if (isPossessive) "+" else ""}")

case class ZeroOrMore(
    expr: RegexTree,
    override val location: Location,
    isReluctant: Boolean = false,
    isPossessive: Boolean = false
) extends Node(expr)(location)(postfix = s"*${if (isReluctant) "?" else if (isPossessive) "+" else ""}")

case class OneOrMore(
    expr: RegexTree,
    override val location: Location,
    isReluctant: Boolean = false,
    isPossessive: Boolean = false
) extends Node(expr)(location)(postfix = s"+${if (isReluctant) "?" else if (isPossessive) "+" else ""}")

case class Concat(nodes: Seq[RegexTree], override val location: Location) extends Node(nodes: _*)(location)

case class Or(nodes: Seq[RegexTree], override val location: Location) extends Node(nodes: _*)(location)(sep = "|")
