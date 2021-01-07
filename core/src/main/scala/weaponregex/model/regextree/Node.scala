package weaponregex.model.regextree

import weaponregex.model.Location

abstract class Node(
    override val children: Seq[RegexTree],
    override val location: Location,
    override val prefix: String = "",
    override val postfix: String = "",
    val sep: String = ""
) extends RegexTree {
  override def buildWith(child: RegexTree, childString: String): String =
    children.map(c => if (c eq child) childString else c.build).mkString(prefix, sep, postfix)

  override def buildWhile(pred: RegexTree => Boolean): String =
    children.filter(pred).map(_.build).mkString(prefix, sep, postfix)
}

case class CharacterClass(nodes: Seq[RegexTree], override val location: Location, isPositive: Boolean = true)
    extends Node(nodes, location, if (isPositive) "[" else "[^", "]") {}

case class Range(from: Character, to: Character, override val location: Location)
    extends Node(Seq(from, to), location, sep = "-")

case class Group(
    expr: RegexTree,
    isCapturing: Boolean,
    override val location: Location
) extends Node(Seq(expr), location, if (isCapturing) "(" else "(?:", ")")

case class NamedGroup(expr: RegexTree, name: String, override val location: Location)
    extends Node(Seq(expr), location, s"(?<$name>", ")")

// Non-capturing group with flags
case class FlagNCGroup(
    flagToggle: FlagToggle,
    expr: RegexTree,
    override val location: Location
) extends Node(Seq(flagToggle, expr), location, "(?", ")", ":")

case class FlagToggleGroup(flagToggle: FlagToggle, override val location: Location)
    extends Node(Seq(flagToggle), location, "(?", ")")

case class FlagToggle(onFlags: Flags, hasDash: Boolean, offFlags: Flags, override val location: Location)
    extends Node(Seq(onFlags, offFlags), location) {
  override lazy val build: String = onFlags.build + (if (hasDash) "-" else "") + offFlags.build
}

case class Flags(flags: Seq[Character], override val location: Location) extends Node(flags, location)

case class Lookaround(expr: RegexTree, isPositive: Boolean, isLookahead: Boolean, override val location: Location)
    extends Node(
      Seq(expr),
      location,
      "(?"
        + (if (isLookahead) "" else "<")
        + (if (isPositive) "=" else "!"),
      ")"
    )

// Independent non-capturing group
case class INCGroup(expr: RegexTree, override val location: Location) extends Node(Seq(expr), location, "(?>", ")")

sealed abstract class QuantifierType(syntax: String) {
  override def toString: String = syntax
}
case object GreedyQuantifier extends QuantifierType("")
case object ReluctantQuantifier extends QuantifierType("?")
case object PossessiveQuantifier extends QuantifierType("+")

case class Quantifier private (
    expr: RegexTree,
    min: Int,
    max: Int,
    override val location: Location,
    quantifierType: QuantifierType,
    isExact: Boolean
) extends Node(
      Seq(expr),
      location,
      postfix = s"{$min"
        + (if (isExact) "" else "," + (if (max < 0) "" else max))
        + s"}$quantifierType"
    )

object Quantifier {
  // Infinity will be represented as negatives, preferably -1
  val Infinity: Int = -1

  // Exact quantifier {n} factory method
  def apply(
      expr: RegexTree,
      exact: Int,
      location: Location,
      quantifierType: QuantifierType
  ): Quantifier = Quantifier(expr, exact, exact, location, quantifierType, isExact = true)

  // Range quantifier {min,max} factory method
  def apply(
      expr: RegexTree,
      min: Int,
      max: Int,
      location: Location,
      quantifierType: QuantifierType
  ): Quantifier = Quantifier(expr, min, max, location, quantifierType, isExact = false)
}

case class ZeroOrOne(
    expr: RegexTree,
    override val location: Location,
    quantifierType: QuantifierType
) extends Node(Seq(expr), location, postfix = s"?$quantifierType")

case class ZeroOrMore(
    expr: RegexTree,
    override val location: Location,
    quantifierType: QuantifierType
) extends Node(Seq(expr), location, postfix = s"*$quantifierType")

case class OneOrMore(
    expr: RegexTree,
    override val location: Location,
    quantifierType: QuantifierType
) extends Node(Seq(expr), location, postfix = s"+$quantifierType")

case class Concat(nodes: Seq[RegexTree], override val location: Location) extends Node(nodes, location)

case class Or(nodes: Seq[RegexTree], override val location: Location) extends Node(nodes, location, sep = "|")
