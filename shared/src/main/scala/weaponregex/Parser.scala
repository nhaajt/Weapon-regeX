package weaponregex

import fastparse._, NoWhitespace._
import weaponregex.model._
import weaponregex.model.regextree._
import weaponregex.extension.StringExtension.StringIndexExtension

object Parser {
  private var currentPattern: String = _

  def Indexed[_: P, T](p: => P[T]): P[(Location, T)] = P(Index ~ p ~ Index)
    .map { case (i, t, j) => (currentPattern.locationOf(i, j), t) }

  def character[_: P]: P[Character] = Indexed(AnyChar.!)
    .map { case (loc, c) => Character(c.head, loc) }

  def any[_: P]: P[Any] = Indexed(P("."))
    .map { case (loc, _) => Any(loc) }

  def bol[_: P]: P[BOL] = Indexed(P("^"))
    .map { case (loc, _) => BOL(loc) }

  def eol[_: P]: P[EOL] = Indexed(P("$"))
    .map { case (loc, _) => EOL(loc) }

  def boundary[_: P]: P[RegexTree] = P(bol | eol)

  def range[_: P]: P[Range] = Indexed(character ~ "-" ~ character)
    .map { case (loc, (from, to)) => Range(from, to, loc) }

  // !! unsupported (yet)
  // Character class item intersection is Scala/Java only
  def classItemIntersection[_: P]: P[ClassItemIntersection] = Indexed(classItem.rep(2, sep = "&&"))
    .map { case (loc, nodes) => ClassItemIntersection(nodes, loc) }

  // Nested character class is Scala/Java only
  def classItem[_: P]: P[RegexTree] = P(range | charClass | character)

  def positiveCharClass[_: P]: P[CharacterClass] = Indexed("[" ~ classItem.rep(1) ~ "]")
    .map { case (loc, nodes) => CharacterClass(nodes, loc) }

  def negativeCharClass[_: P]: P[CharacterClass] = Indexed("[^" ~ classItem.rep(1) ~ "]")
    .map { case (loc, nodes) => CharacterClass(nodes, loc, isPositive = false) }

  def charClass[_: P]: P[CharacterClass] = P(positiveCharClass | negativeCharClass)

  // ! unfinished
  def elementaryRE[_: P]: P[RegexTree] = P(boundary | charClass | character)

  // ! missing quantifier
  def basicRE[_: P]: P[RegexTree] = P(elementaryRE)

  def concat[_: P]: P[Concat] = Indexed(basicRE.rep(2))
    .map { case (loc, nodes) => Concat(nodes, loc) }

  def simpleRE[_: P]: P[RegexTree] = P(concat | basicRE)

  def or[_: P]: P[Or] = Indexed(simpleRE.rep(2, sep = "|"))
    .map { case (loc, nodes) => Or(nodes, loc) }

  def RE[_: P]: P[RegexTree] = P(or | simpleRE)

  final def apply(pattern: String): Option[RegexTree] = parse(pattern)

  def parse(pattern: String): Option[RegexTree] = {
    currentPattern = pattern

    fastparse.parse(pattern, RE(_)) match {
      case Parsed.Success(regexTree: RegexTree, index) => Some(regexTree)
      case Parsed.Failure(str, index, extra)           => None
    }
  }

  def parseOrError(pattern: String): RegexTree =
    parse(pattern).getOrElse(throw new RuntimeException("Failed to parse regex"))
}
