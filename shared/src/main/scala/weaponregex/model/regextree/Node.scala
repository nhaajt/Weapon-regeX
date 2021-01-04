package weaponregex.model.regextree

import weaponregex.model.Location

abstract class Node(override val children: RegexTree*)(override val location: Location)(implicit
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
    extends Node(nodes: _*)(location)(if (isPositive) "[" else "[^", "]") {}

case class Range(from: Character, to: Character, override val location: Location)
    extends Node(from, to)(location)(sep = "-")

case class Group(
    expr: RegexTree,
    isCapturing: Boolean,
    override val location: Location
) extends Node(expr)(location)(if (isCapturing) "(" else "(?:", ")")

case class NamedGroup(expr: RegexTree, name: String, override val location: Location)
    extends Node(expr)(location)(s"(?<$name>", ")")

case class FlagNCGroup(
    flagToggle: FlagToggle,
    expr: RegexTree,
    override val location: Location
) extends Node(flagToggle, expr)(location)("(?", ")", ":")

case class FlagGroup(flagToggle: FlagToggle, override val location: Location)
    extends Node(flagToggle)(location)("(?", ")")

// Non-capturing group with flags
case class FlagToggle(onFlags: Flags, hasDash: Boolean, offFlags: Flags, override val location: Location)
    extends Node(onFlags, offFlags)(location) {
  override lazy val build: String = onFlags.build + (if (hasDash) "-" else "") + offFlags.build
}

case class Flags(flags: Seq[Character], override val location: Location) extends Node(flags: _*)(location)

case class Lookaround(expr: RegexTree, isPositive: Boolean, isLookahead: Boolean, override val location: Location)
    extends Node(expr)(location)(
      "(?"
        + (if (isLookahead) "" else "<")
        + (if (isPositive) "=" else "!"),
      ")"
    )

// Independent non-capturing group
case class INCGroup(expr: RegexTree, override val location: Location) extends Node(expr)(location)("(?>", ")")

object QuantifierType extends Enumeration {
  type QuantifierType = Value

  val Greedy: QuantifierType = Value("")
  val Reluctant: QuantifierType = Value("?")
  val Possessive: QuantifierType = Value("+")
}

case class Quantifier private (
    expr: RegexTree,
    min: Int,
    max: Int,
    override val location: Location,
    quantifierType: QuantifierType.Value,
    isExact: Boolean
) extends Node(expr)(location)(
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
      quantifierType: QuantifierType.Value
  ): Quantifier = Quantifier(expr, exact, exact, location, quantifierType, isExact = true)

  // Range quantifier {min,max} factory method
  def apply(
      expr: RegexTree,
      min: Int,
      max: Int,
      location: Location,
      quantifierType: QuantifierType.Value
  ): Quantifier = Quantifier(expr, min, max, location, quantifierType, isExact = false)
}

case class ZeroOrOne(
    expr: RegexTree,
    override val location: Location,
    quantifierType: QuantifierType.Value
) extends Node(expr)(location)(postfix = s"?$quantifierType")

case class ZeroOrMore(
    expr: RegexTree,
    override val location: Location,
    quantifierType: QuantifierType.Value
) extends Node(expr)(location)(postfix = s"*$quantifierType")

case class OneOrMore(
    expr: RegexTree,
    override val location: Location,
    quantifierType: QuantifierType.Value
) extends Node(expr)(location)(postfix = s"+$quantifierType")

case class Concat(nodes: Seq[RegexTree], override val location: Location) extends Node(nodes: _*)(location)

case class Or(nodes: Seq[RegexTree], override val location: Location) extends Node(nodes: _*)(location)(sep = "|")
