package weaponregex.parser

import weaponregex.model.regextree._

class ParserTest extends munit.FunSuite {
  def treeBuildTest(tree: RegexTree, pattern: String): Unit = assertEquals(tree.build, pattern)

  test("Parse concat of characters") {
    val pattern = "hello"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[Concat])
    parsedTree.children foreach (child => assert(child.isInstanceOf[Character], clue = parsedTree.children))

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

  test("Parse boundary metacharacters") {
    val pattern = """\b\B\A\G\z\Z"""
    val parsedTree = Parser.parseOrError(pattern)
    parsedTree.children foreach (child => assert(child.isInstanceOf[Boundary], clue = parsedTree.children))

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
        case c: Character => c.char == 'a'
        case _            => false
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
    parsedTree.children foreach (child => assert(child.isInstanceOf[Range], clue = parsedTree.children))

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse character class with nested character classes") {
    val pattern = "[[a-z][^A-Z0-9][01234]]"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[CharacterClass])
    parsedTree.children foreach (child => assert(child.isInstanceOf[CharacterClass], clue = parsedTree.children))

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse character class with predefined character classes") {
    val pattern = """[\w\W\s\S\d\D]"""
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[CharacterClass])
    parsedTree.children foreach (child => assert(child.isInstanceOf[PredefinedCharClass], clue = parsedTree.children))

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse escape characters") {
    val pattern = """\\\t\n\r\f"""
    val parsedTree = Parser.parseOrError(pattern)
    parsedTree.children foreach (child => assert(child.isInstanceOf[MetaChar], clue = parsedTree.children))

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse hexadecimal characters") {
    val pattern = "\\x20\\u0020\\x{000020}"
    val parsedTree = Parser.parseOrError(pattern)
    parsedTree.children foreach (child => assert(child.isInstanceOf[MetaChar], clue = parsedTree.children))

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse octal characters") {
    val pattern = """\01\012\0123"""
    val parsedTree = Parser.parseOrError(pattern)
    parsedTree.children foreach (child => assert(child.isInstanceOf[MetaChar], clue = parsedTree.children))

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse predefined character class") {
    val pattern = """.\w\W\s\S\d\D"""
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children.head).isInstanceOf[AnyDot])
    parsedTree.children.tail foreach (child =>
      assert(child.isInstanceOf[PredefinedCharClass], clue = parsedTree.children.tail)
    )

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse short greedy quantifiers") {
    val pattern = "a*a+a?"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children(0)).isInstanceOf[ZeroOrMore])
    assert(clue(parsedTree.children(1)).isInstanceOf[OneOrMore])
    assert(clue(parsedTree.children(2)).isInstanceOf[ZeroOrOne])
    parsedTree.children foreach (child =>
      assert(
        (child match {
          case q: ZeroOrMore => q.quantifierType
          case q: OneOrMore  => q.quantifierType
          case q: ZeroOrOne  => q.quantifierType
        }) == GreedyQuantifier,
        clue = parsedTree.children
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
    parsedTree.children foreach (child =>
      assert(
        (child match {
          case q: ZeroOrMore => q.quantifierType
          case q: OneOrMore  => q.quantifierType
          case q: ZeroOrOne  => q.quantifierType
        }) == ReluctantQuantifier,
        clue = parsedTree.children
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
    parsedTree.children foreach (child =>
      assert(
        (child match {
          case q: ZeroOrMore => q.quantifierType
          case q: OneOrMore  => q.quantifierType
          case q: ZeroOrOne  => q.quantifierType
        }) == PossessiveQuantifier,
        clue = parsedTree.children
      )
    )

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse long greedy quantifiers") {
    val pattern = "a{1}a{1,}a{1,2}"
    val parsedTree = Parser.parseOrError(pattern)
    parsedTree.children foreach (child =>
      assert(
        child match {
          case q: Quantifier => q.quantifierType == GreedyQuantifier
          case _             => false
        },
        clue = parsedTree.children
      )
    )

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse long reluctant quantifiers") {
    val pattern = "a{1}?a{1,}?a{1,2}?"
    val parsedTree = Parser.parseOrError(pattern)
    parsedTree.children foreach (child =>
      assert(
        child match {
          case q: Quantifier => q.quantifierType == ReluctantQuantifier
          case _             => false
        },
        clue = parsedTree.children
      )
    )

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse long possessive quantifiers") {
    val pattern = "a{1}+a{1,}+a{1,2}+"
    val parsedTree = Parser.parseOrError(pattern)
    parsedTree.children foreach (child =>
      assert(
        child match {
          case q: Quantifier => q.quantifierType == PossessiveQuantifier
          case _             => false
        },
        clue = parsedTree.children
      )
    )

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse capturing group") {
    val pattern = "(hello)(world)"
    val parsedTree = Parser.parseOrError(pattern)
    parsedTree.children foreach (child => assert(child.isInstanceOf[Group], clue = parsedTree.children))

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse named capturing group") {
    val pattern = "(?<name1>hello)(?<name2>world)"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children(0)) match {
      case NamedGroup(_, name, _) => name == "name1"
      case _                      => false
    })
    assert(clue(parsedTree.children(1)) match {
      case NamedGroup(_, name, _) => name == "name2"
      case _                      => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse non-capturing group") {
    val pattern = "(?:hello)(?:world)"
    val parsedTree = Parser.parseOrError(pattern)
    parsedTree.children foreach (child =>
      assert(
        child match {
          case Group(_, false, _) => true
          case _                  => false
        },
        clue = parsedTree.children
      )
    )

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse flag toggle group i-i") {
    val pattern = "(?idmsuxU-idmsuxU)"
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case FlagToggleGroup(FlagToggle(onFlags, hasDash, offFlags, _), _) =>
        onFlags.flags.nonEmpty && offFlags.flags.nonEmpty && hasDash
      case _ => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse flag toggle group i-") {
    val pattern = "(?idmsuxU-)"
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case FlagToggleGroup(FlagToggle(onFlags, hasDash, offFlags, _), _) =>
        onFlags.flags.nonEmpty && offFlags.flags.isEmpty && hasDash
      case _ => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse flag toggle group -i") {
    val pattern = "(?-idmsuxU)"
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case FlagToggleGroup(FlagToggle(onFlags, hasDash, offFlags, _), _) =>
        onFlags.flags.isEmpty && offFlags.flags.nonEmpty && hasDash
      case _ => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse flag toggle group -") {
    val pattern = "(?-)"
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case FlagToggleGroup(FlagToggle(onFlags, hasDash, offFlags, _), _) =>
        onFlags.flags.isEmpty && offFlags.flags.isEmpty && hasDash
      case _ => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse flag toggle group i") {
    val pattern = "(?idmsuxU)"
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case FlagToggleGroup(FlagToggle(onFlags, hasDash, offFlags, _), _) =>
        onFlags.flags.nonEmpty && offFlags.flags.isEmpty && !hasDash
      case _ => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse non-capturing group with flags i-i") {
    val pattern = "(?idmsux-idmsux:hello)"
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case FlagNCGroup(FlagToggle(onFlags, hasDash, offFlags, _), _, _) =>
        onFlags.flags.nonEmpty && offFlags.flags.nonEmpty && hasDash
      case _ => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse non-capturing group with flags i-") {
    val pattern = "(?idmsux-:hello)"
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case FlagNCGroup(FlagToggle(onFlags, hasDash, offFlags, _), _, _) =>
        onFlags.flags.nonEmpty && offFlags.flags.isEmpty && hasDash
      case _ => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse non-capturing group with flags -i") {
    val pattern = "(?-idmsux:hello)"
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case FlagNCGroup(FlagToggle(onFlags, hasDash, offFlags, _), _, _) =>
        onFlags.flags.isEmpty && offFlags.flags.nonEmpty && hasDash
      case _ => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse non-capturing group with flags -") {
    val pattern = "(?-:hello)"
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case FlagNCGroup(FlagToggle(onFlags, hasDash, offFlags, _), _, _) =>
        onFlags.flags.isEmpty && offFlags.flags.isEmpty && hasDash
      case _ => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse non-capturing group with flags i") {
    val pattern = "(?idmsux:hello)"
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case FlagNCGroup(FlagToggle(onFlags, hasDash, offFlags, _), _, _) =>
        onFlags.flags.nonEmpty && offFlags.flags.isEmpty && !hasDash
      case _ => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse positive lookahead") {
    val pattern = "(?=hello)"
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case Lookaround(_, true, true, _) => true
      case _                            => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse negative lookahead") {
    val pattern = "(?!hello)"
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case Lookaround(_, false, true, _) => true
      case _                             => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse positive lookbehind") {
    val pattern = "(?<=hello)"
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case Lookaround(_, true, false, _) => true
      case _                             => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse negative lookbehind") {
    val pattern = "(?<!hello)"
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case Lookaround(_, false, false, _) => true
      case _                              => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse independent non-capturing group") {
    val pattern = "(?>hello)"
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree).isInstanceOf[INCGroup])

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse named reference") {
    val pattern = """\k<name1>"""
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case NameReference("name1", _) => true
      case _                         => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse numbered reference") {
    val pattern = """\123"""
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case NumberReference(123, _) => true
      case _                       => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse character quote") {
    val pattern = """\$"""
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree) match {
      case QuoteChar('$', _) => true
      case _                 => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse long quote with end") {
    val pattern = """stuff\Q$hit\Emorestuff"""
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree.children(5)) match {
      case Quote("$hit", true, _) => true
      case _                      => false
    })

    treeBuildTest(parsedTree, pattern)
  }

  test("Parse long quote without end") {
    val pattern = """stuff\Q$hit"""
    val parsedTree = Parser.parseOrError(pattern)

    assert(clue(parsedTree.children(5)) match {
      case Quote("$hit", false, _) => true
      case _                       => false
    })

    treeBuildTest(parsedTree, pattern)
  }
}
