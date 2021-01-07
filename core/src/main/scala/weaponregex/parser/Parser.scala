package weaponregex.parser

import fastparse._, NoWhitespace._
import weaponregex.model._
import weaponregex.model.regextree._
import weaponregex.extension.StringExtension.StringIndexExtension

/** Companion object for [[weaponregex.parser.Parser]] class that instantiates [[weaponregex.parser.Parser]] instances
  */
object Parser {

  /** Apply the parser to parse the given pattern
    * @param pattern The regex pattern to be parsed
    * @return An `Option`al parsed [[weaponregex.model.regextree.RegexTree]]
    */
  def apply(pattern: String): Option[RegexTree] = new Parser(pattern).parse

  /** Apply the parser to parse the given pattern
    * @param pattern The regex pattern to be parsed
    * @return [[weaponregex.model.regextree.RegexTree]] if the given pattern can ber parsed, throw an error otherwise
    */
  def parseOrError(pattern: String): RegexTree =
    new Parser(pattern).parse.getOrElse(throw new RuntimeException("Failed to parse regex"))
}

/** @param pattern The regex pattern to be parsed
  * @note This class constructor is private, instances must be created using the companion [[weaponregex.parser.Parser]] object
  * @note The parsing rules methods inside this class is created based on the defined grammar
  */
class Parser private (val pattern: String) {
  final private val specialChars: String = """[](){}\.^$|?*+"""

  def Indexed[_: P, T](p: => P[T]): P[(Location, T)] = P(Index ~ p ~ Index)
    .map { case (i, t, j) => (pattern.locationOf(i, j), t) }

  def number[_: P]: P[Int] = P(CharIn("0-9").rep(1).!) map (_.toInt)

  def charLiteral[_: P]: P[Character] = Indexed(CharPred(!specialChars.contains(_)).!)
    .map { case (loc, c) => Character(c.head, loc) }

  def character[_: P]: P[RegexTree] = P(metaCharacter | charLiteral)

  def bol[_: P]: P[BOL] = Indexed(P("^"))
    .map { case (loc, _) => BOL(loc) }

  def eol[_: P]: P[EOL] = Indexed(P("$"))
    .map { case (loc, _) => EOL(loc) }

  def boundaryMetaChar[_: P]: P[Boundary] = Indexed("""\""" ~ CharIn("bBAGzZ").!)
    .map { case (loc, b) => Boundary(b, loc) }

  def boundary[_: P]: P[RegexTree] = P(bol | eol | boundaryMetaChar)

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

  // Nested character class is Scala/Java only
  def classItem[_: P]: P[RegexTree] = P(range | charClass | preDefinedCharClass | charLiteral)

  def positiveCharClass[_: P]: P[CharacterClass] = Indexed("[" ~ classItem.rep(1) ~ "]")
    .map { case (loc, nodes) => CharacterClass(nodes, loc) }

  def negativeCharClass[_: P]: P[CharacterClass] = Indexed("[^" ~ classItem.rep(1) ~ "]")
    .map { case (loc, nodes) => CharacterClass(nodes, loc, isPositive = false) }

  def charClass[_: P]: P[CharacterClass] = P(positiveCharClass | negativeCharClass)

  def anyDot[_: P]: P[AnyDot] = Indexed(P("."))
    .map { case (loc, _) => AnyDot(loc) }

  def preDefinedCharClass[_: P]: P[PredefinedCharClass] = Indexed("""\""" ~ CharIn("dDsSwW").!)
    .map { case (loc, c) => PredefinedCharClass(c, loc) }

  def quantifierType[_: P, T](p: => P[T]): P[(T, QuantifierType)] = P(p ~ CharIn("?+").!.?)
    .map { case (pp, optionQType) =>
      (
        pp,
        optionQType match {
          case Some("?") => ReluctantQuantifier
          case Some("+") => PossessiveQuantifier
          case _         => GreedyQuantifier
        }
      )
    }

  def quantifierShort[_: P]: P[RegexTree] = Indexed(quantifierType(elementaryRE ~ CharIn("?*+").!))
    .map { case (loc, ((expr, q), quantifierType)) =>
      q match {
        case "?" => ZeroOrOne(expr, loc, quantifierType)
        case "*" => ZeroOrMore(expr, loc, quantifierType)
        case "+" => OneOrMore(expr, loc, quantifierType)
      }
    }

  def quantifierLong[_: P]: P[Quantifier] =
    Indexed(quantifierType(elementaryRE ~ "{" ~ number ~ ("," ~ number.?).? ~ "}"))
      .map { case (loc, ((expr, num, optionMax), quantifierType)) =>
        optionMax match {
          case None            => Quantifier(expr, num, loc, quantifierType)
          case Some(None)      => Quantifier(expr, num, Quantifier.Infinity, loc, quantifierType)
          case Some(Some(max)) => Quantifier(expr, num, max, loc, quantifierType)
        }
      }

  def quantifier[_: P]: P[RegexTree] = P(quantifierShort | quantifierLong)

  def group[_: P]: P[Group] = Indexed("(" ~ RE ~ ")")
    .map { case (loc, expr) => Group(expr, isCapturing = true, loc) }

  def groupName[_: P]: P[String] = P(CharIn("a-z", "A-Z") ~ CharIn("a-z", "A-Z", "0-9").rep).!

  def namedGroup[_: P]: P[NamedGroup] = Indexed("(?<" ~ groupName ~ ">" ~ RE ~ ")")
    .map { case (loc, (name, expr)) => NamedGroup(expr, name, loc) }

  def nonCapturingGroup[_: P]: P[Group] = Indexed("(?:" ~ RE ~ ")")
    .map { case (loc, expr) => Group(expr, isCapturing = false, loc) }

  // `.filter()` function from fastparse is wrongly mutated by Stryker4s into `.filterNot()` which does not exist in fastparse
  @SuppressWarnings(Array("stryker4s.mutation.MethodExpression"))
  def flags[_: P](fs: String): P[Flags] = Indexed(charLiteral.filter(c => fs.contains(c.char)).rep)
    .map { case (loc, fs) => Flags(fs, loc) }

  def flagToggle[_: P](fs: String): P[FlagToggle] = Indexed(flags(fs) ~ "-".!.? ~ flags(fs))
    .map { case (loc, (onFlags, dash, offFlags)) => FlagToggle(onFlags, dash.isDefined, offFlags, loc) }

  def flagToggleGroup[_: P]: P[FlagToggleGroup] = Indexed("(?" ~ flagToggle("idmsuxU") ~ ")")
    .map { case (loc, ft) => FlagToggleGroup(ft, loc) }

  def flagNCGroup[_: P]: P[FlagNCGroup] = Indexed("(?" ~ flagToggle("idmsux") ~ ":" ~ RE ~ ")")
    .map { case (loc, (ft, expr)) => FlagNCGroup(ft, expr, loc) }

  def lookaround[_: P]: P[Lookaround] = Indexed("(?" ~ "<".!.? ~ CharIn("=!").! ~ RE ~ ")")
    .map { case (loc, (angleBracket, posNeg, expr)) => Lookaround(expr, posNeg == "=", angleBracket.isEmpty, loc) }

  def incGroup[_: P]: P[INCGroup] = Indexed("(?>" ~ RE ~ ")")
    .map { case (loc, expr) => INCGroup(expr, loc) }

  def specialConstruct[_: P]: P[RegexTree] = P(
    namedGroup | nonCapturingGroup | flagToggleGroup | flagNCGroup | lookaround | incGroup
  )

  def capturing[_: P]: P[RegexTree] = P(group | specialConstruct)

  def nameReference[_: P]: P[NameReference] = Indexed("""\k<""" ~ groupName ~ ">")
    .map { case (loc, name) => NameReference(name, loc) }

  def numReference[_: P]: P[NumberReference] = Indexed("""\""" ~ (CharIn("1-9") ~ CharIn("0-9").rep).!)
    .map { case (loc, num) => NumberReference(num.toInt, loc) }

  def reference[_: P]: P[RegexTree] = P(nameReference | numReference)

  def quoteChar[_: P]: P[RegexTree] = Indexed("""\""" ~ AnyChar.!)
    .map { case (loc, char) => QuoteChar(char.head, loc) }

  def quoteLong[_: P]: P[RegexTree] = Indexed("""\Q""" ~ (!"""\E""" ~ AnyChar).rep.! ~ """\E""".!.?)
    .map { case (loc, (str, end)) => Quote(str, end.isDefined, loc) }

  def quote[_: P]: P[RegexTree] = P(quoteLong | quoteChar)

  def elementaryRE[_: P]: P[RegexTree] = P(
    capturing | anyDot | preDefinedCharClass | boundary | charClass | reference | character | quote
  )

  def basicRE[_: P]: P[RegexTree] = P(quantifier | elementaryRE)

  def concat[_: P]: P[Concat] = Indexed(basicRE.rep(2))
    .map { case (loc, nodes) => Concat(nodes, loc) }

  def simpleRE[_: P]: P[RegexTree] = P(concat | basicRE)

  def or[_: P]: P[Or] = Indexed(simpleRE.rep(2, sep = "|"))
    .map { case (loc, nodes) => Or(nodes, loc) }

  def RE[_: P]: P[RegexTree] = P(or | simpleRE)

  /** Parse the given regex pattern
    * @return An `Option`al parsed [[weaponregex.model.regextree.RegexTree]]
    */
  def parse: Option[RegexTree] = fastparse.parse(pattern, RE(_)) match {
    case Parsed.Success(regexTree: RegexTree, index) => Some(regexTree)
    case f @ Parsed.Failure(str, index, extra) =>
      println(str)
      println(index)
      println(f.msg)
      None
  }
}
