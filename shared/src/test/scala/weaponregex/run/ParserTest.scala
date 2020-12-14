package weaponregex.run

import weaponregex.model.regextree._

class ParserTest extends munit.FunSuite {
  def treeBuildTest(tree: RegexTree, pattern: String): Unit = assertEquals(tree.build, pattern)

  test("Parse concat of characters") {
    val pattern = "hello"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[Concat])
    // Small note, but for better error messages something like `parsedTree.children.foreach(assert(_.isInstanceOf[Character]))` would be more helpful
    assert(clue(parsedTree.children) forall (_.isInstanceOf[Character]))

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse or of characters") {
    val pattern = "h|e|l|l|o"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[Or])

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse BOL and EOL") {
    val pattern = "^hello$"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[Concat])
    assert(clue(parsedTree.children.head).isInstanceOf[BOL])
    assert(clue(parsedTree.children.last).isInstanceOf[EOL])

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse multiple lines with location") {
    val pattern =
      """a
        |a
        |a
        |a
        |a
        |a""".stripMargin
    val parsedTree = Parser.parseOrError(pattern)
    assertEquals(
      parsedTree.children filter {
        case Character('a', _) => true
        case _                 => false
      } map (_.location.start.line),
      0 to 5
    )

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse character class with characters") {
    val pattern = "[abc]"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[CharacterClass])

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse negative character class with characters") {
    val pattern = "[^abc]"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[CharacterClass])

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse character class with ranges") {
    val pattern = "[a-zA-Z0-9]"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[CharacterClass])
    assert(clue(parsedTree.children) forall (_.isInstanceOf[Range]))

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse character class with nested character classes") {
    val pattern = "[[a-z][^A-Z0-9][01234]]"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[CharacterClass])
    assert(clue(parsedTree.children) forall (_.isInstanceOf[CharacterClass]))

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse escape characters") {
    val pattern = """\\\t\n\r\f"""
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children) forall (_.isInstanceOf[MetaChar]))

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse hexadecimal characters") {
    val pattern = "\\x20\\u0020\\x{000020}"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children) forall (_.isInstanceOf[MetaChar]))

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse octal characters") {
    val pattern = """\01\012\0123"""
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children) forall (_.isInstanceOf[MetaChar]))

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse predefined character class") {
    val pattern = """.\w\W\s\S\d\D"""
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children.head).isInstanceOf[Any])
    assert(clue(parsedTree.children.tail) forall (_.isInstanceOf[PredefinedCharClass]))

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse short greedy quantifiers") {
    val pattern = "a*a+a?"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children(0)).isInstanceOf[ZeroOrMore])
    assert(clue(parsedTree.children(1)).isInstanceOf[OneOrMore])
    assert(clue(parsedTree.children(2)).isInstanceOf[ZeroOrOne])
    assert(
      clue(parsedTree.children) forall (child =>
        (child match {
          case q: ZeroOrMore => q.quantifierType
          case q: OneOrMore  => q.quantifierType
          case q: ZeroOrOne  => q.quantifierType
        }) == QuantifierType.Greedy
      )
    )

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse short reluctant quantifiers") {
    val pattern = "a*?a+?a??"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children(0)).isInstanceOf[ZeroOrMore])
    assert(clue(parsedTree.children(1)).isInstanceOf[OneOrMore])
    assert(clue(parsedTree.children(2)).isInstanceOf[ZeroOrOne])
    assert(
      clue(parsedTree.children) forall (child =>
        (child match {
          case q: ZeroOrMore => q.quantifierType
          case q: OneOrMore  => q.quantifierType
          case q: ZeroOrOne  => q.quantifierType
        }) == QuantifierType.Reluctant
      )
    )

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse short possessive quantifiers") {
    val pattern = "a*+a++a?+"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children(0)).isInstanceOf[ZeroOrMore])
    assert(clue(parsedTree.children(1)).isInstanceOf[OneOrMore])
    assert(clue(parsedTree.children(2)).isInstanceOf[ZeroOrOne])
    assert(
      clue(parsedTree.children) forall (child =>
        (child match {
          case q: ZeroOrMore => q.quantifierType
          case q: OneOrMore  => q.quantifierType
          case q: ZeroOrOne  => q.quantifierType
        }) == QuantifierType.Possessive
      )
    )

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse long greedy quantifiers") {
    val pattern = "a{1}a{1,}a{1,2}"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children) forall {
      case q: Quantifier => q.quantifierType == QuantifierType.Greedy
      case _             => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse long reluctant quantifiers") {
    val pattern = "a{1}?a{1,}?a{1,2}?"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children) forall {
      case q: Quantifier => q.quantifierType == QuantifierType.Reluctant
      case _             => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse long possessive quantifiers") {
    val pattern = "a{1}+a{1,}+a{1,2}+"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children) forall {
      case q: Quantifier => q.quantifierType == QuantifierType.Possessive
      case _             => false
    })

    treeBuildTest(parsedTree, pattern)
  }
}
