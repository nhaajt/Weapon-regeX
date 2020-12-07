package weaponregex.run

import fastparse._, NoWhitespace._
import weaponregex.model._
import weaponregex.model.regextree._
import weaponregex.extension.StringExtension.StringIndexExtension
object Parser {
  private var currentPattern: String = _
  private val specialChars: String = """[](){}\.^$|?*+"""

  def Indexed[_: P, T](p: => P[T]): P[(Location, T)] = P(Index ~ p ~ Index)
    .map { case (i, t, j) => (currentPattern.locationOf(i, j), t) }

  def number[_: P]: P[Int] = P(CharIn("0-9").rep(1).!.map(_.toInt))

  def charLiteral[_: P]: P[Character] = Indexed(CharPred(!specialChars.contains(_)).!)
    .map { case (loc, c) => Character(c.head, loc) }

  def character[_: P]: P[RegexTree] = P(metaCharacter | charLiteral)

  def bol[_: P]: P[BOL] = Indexed(P("^"))
    .map { case (loc, _) => BOL(loc) }

  def eol[_: P]: P[EOL] = Indexed(P("$"))
    .map { case (loc, _) => EOL(loc) }

  def boundary[_: P]: P[RegexTree] = P(bol | eol)

  def metaCharacter[_: P]: P[RegexTree] = P(charOct | charHex | charUnicode | charHexBrace | escapeChar)

  // \c is not supported yet
  // \a an \e is JVM only
  def escapeChar[_: P]: P[MetaChar] =
    Indexed("""\""" ~ CharIn("\\\\tnrf").!) // fastparse needs //// for a single backslash
      .map { case (loc, c) => MetaChar(c, loc) }

  def charOct[_: P]: P[MetaChar] = Indexed("""\0""" ~ CharIn("0-7").!.rep(min = 1, max = 3))
    .map { case (loc, octDigits) => MetaChar("0" + octDigits.mkString, loc) }

  def charHex[_: P]: P[MetaChar] = Indexed("""\x""" ~ CharIn("0-9a-zA-Z").!.rep(exactly = 2))
    .map { case (loc, hexDigits) => MetaChar("x" + hexDigits.mkString, loc) }

  def charUnicode[_: P]: P[MetaChar] = Indexed("\\u" ~ CharIn("0-9a-zA-Z").!.rep(exactly = 4))
    .map { case (loc, hexDigits) => MetaChar("u" + hexDigits.mkString, loc) }

  def charHexBrace[_: P]: P[MetaChar] = Indexed("""\x{""" ~ CharIn("0-9a-zA-Z").!.rep(1) ~ "}")
    .map { case (loc, hexDigits) => MetaChar("x{" + hexDigits.mkString + "}", loc) }

  def range[_: P]: P[Range] = Indexed(charLiteral ~ "-" ~ charLiteral)
    .map { case (loc, (from, to)) => Range(from, to, loc) }

  // !! unsupported (yet)
  // Character class item intersection is Scala/Java only
  def classItemIntersection[_: P]: P[ClassItemIntersection] = Indexed(classItem.rep(2, sep = "&&"))
    .map { case (loc, nodes) => ClassItemIntersection(nodes, loc) }

  // Nested character class is Scala/Java only
  def classItem[_: P]: P[RegexTree] = P(range | charClass | charLiteral)

  def positiveCharClass[_: P]: P[CharacterClass] = Indexed("[" ~ classItem.rep(1) ~ "]")
    .map { case (loc, nodes) => CharacterClass(nodes, loc) }

  def negativeCharClass[_: P]: P[CharacterClass] = Indexed("[^" ~ classItem.rep(1) ~ "]")
    .map { case (loc, nodes) => CharacterClass(nodes, loc, isPositive = false) }

  def charClass[_: P]: P[CharacterClass] = P(positiveCharClass | negativeCharClass)

  def any[_: P]: P[Any] = Indexed(P("."))
    .map { case (loc, _) => Any(loc) }

  def preDefinedCharClass[_: P]: P[PredefinedCharClass] = Indexed("""\""" ~ CharIn("dDsSwW").!)
    .map { case (loc, c) => PredefinedCharClass(c, loc) }

  def quantifierType[_: P, T](p: => P[T]): P[(T, QuantifierType.Value)] = P(p ~ CharIn("?+").!.?)
    .map { case (pp, optionQType) =>
      (
        pp,
        optionQType match {
          case Some("?") => QuantifierType.Reluctant
          case Some("+") => QuantifierType.Possessive
          case _         => QuantifierType.Greedy
        }
      )
    }

//  def zeroOrOne[_: P]: P[ZeroOrOne] = Indexed(quantifierType(elementaryRE ~ "?"))
//    .map { case (loc, (expr, quantifierType)) => ZeroOrOne(expr, loc, quantifierType) }
//
//  def zeroOrMore[_: P]: P[ZeroOrMore] = Indexed(quantifierType(elementaryRE ~ "*"))
//    .map { case (loc, (expr, quantifierType)) => ZeroOrMore(expr, loc, quantifierType) }
//
//  def oneOrMore[_: P]: P[OneOrMore] = Indexed(quantifierType(elementaryRE ~ "+"))
//    .map { case (loc, (expr, quantifierType)) => OneOrMore(expr, loc, quantifierType) }
//
//  def quantifierShort[_: P]: P[RegexTree] = P(zeroOrOne | zeroOrMore | oneOrMore)

  def quantifierShort[_: P]: P[RegexTree] = Indexed(quantifierType(elementaryRE ~ CharIn("?*+").!))
    .map { case (loc, ((expr, q), quantifierType)) =>
      q match {
        case "?" => ZeroOrOne(expr, loc, quantifierType)
        case "*" => ZeroOrMore(expr, loc, quantifierType)
        case "+" => OneOrMore(expr, loc, quantifierType)
      }
    }

//  def quantifierExact[_: P]: P[Quantifier] = Indexed(quantifierType(elementaryRE ~ "{" ~ number ~ "}"))
//    .map { case (loc, ((expr, exact), quantifierType)) => Quantifier(expr, exact, loc, quantifierType) }
//
//  def quantifierOnlyMin[_: P]: P[Quantifier] = Indexed(quantifierType(elementaryRE ~ "{" ~ number ~ ",}"))
//    .map { case (loc, ((expr, min), quantifierType)) => Quantifier(expr, min, -1, loc, quantifierType) }
//
//  def quantifierRange[_: P]: P[Quantifier] = Indexed(quantifierType(elementaryRE ~ "{" ~ number ~ "," ~ number ~ "}"))
//    .map { case (loc, ((expr, min, max), quantifierType)) => Quantifier(expr, min, max, loc, quantifierType) }
//
//  def quantifierNum[_: P]: P[Quantifier] = P(quantifierExact | quantifierOnlyMin | quantifierRange)

  def quantifierNum[_: P]: P[Quantifier] =
    Indexed(quantifierType(elementaryRE ~ "{" ~ number ~ ("," ~ number.?).? ~ "}"))
      .map { case (loc, ((expr, num, optionMax), quantifierType)) =>
        optionMax match {
          case None            => Quantifier(expr, num, loc, quantifierType)
          case Some(None)      => Quantifier(expr, num, -1, loc, quantifierType)
          case Some(Some(max)) => Quantifier(expr, num, max, loc, quantifierType)
        }
      }

  def quantifier[_: P]: P[RegexTree] = P(quantifierShort | quantifierNum)

  // x? + * "quantifierAlias"

  //[abc]{3}
  //[abc]{3,}
  //[abc]{3,6}
  // (expr: RegexTree, quantifier: String)
//  def quantifierAll[_: P]: P[RegexTree] = Indexed(
//    P(
//      elementaryRE ~ (
//        CharIn("?*+").! |
//          ("{" ~ number ~ "}").! |
//          ("{" ~ number ~ ",}").! |
//          ("{" ~ number.rep(exactly = 2, sep = ",") ~ "}").!
//      )
//    )
//  ) map { case (loc, (expr, pattern)) =>
//    pattern match {
//      case '?' +: _ => ZeroOrOne(expr, loc, QuantifierType.Greedy)
//      case '+' +: _ => OneOrMore(expr, loc, QuantifierType.Greedy)
//      case '*' +: _ => ZeroOrMore(expr, loc, QuantifierType.Greedy)
//      case pattern if !pattern.contains(',') => {
//        val n = pattern.toList.filter(_.isDigit).toString.toInt
//        Quantifier(expr, exact = n, loc, QuantifierType.Greedy)
//      }
//      case _ :+ ",}" => {
//        val n = pattern.toList.filter(_.isDigit).toString.toInt
//        Quantifier(expr, min = n, max = -1, loc, QuantifierType.Greedy)
//      }
//      case pattern if pattern.contains(',') && !pattern.endsWith(",}") => {
//        val nums = pattern.split(",").map(_.toList.filter(_.isDigit).toString.toInt)
//        Quantifier(expr, min = nums(0), max = nums(1), loc, QuantifierType.Greedy)
//      }
//    }
//  }

  // ! unfinished
  def elementaryRE[_: P]: P[RegexTree] = P(any | preDefinedCharClass | boundary | charClass | character)

  // ! missing quantifier
  def basicRE[_: P]: P[RegexTree] = P(quantifier | elementaryRE)

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
