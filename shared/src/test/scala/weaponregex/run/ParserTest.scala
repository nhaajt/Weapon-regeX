package weaponregex.run

import weaponregex.model.regextree._

class ParserTest extends munit.FunSuite {
  test("Parse concat of characters") {
    val pattern = "hello"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[Concat])
    assert(clue(parsedTree.children) forall (_.isInstanceOf[Character]))

    val buildPattern: String = parsedTree.build
    assertEquals(buildPattern, pattern)
  }

  test("Parse or of characters") {
    val pattern = "h|e|l|l|o"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[Or])

    val buildPattern: String = parsedTree.build
    assertEquals(buildPattern, pattern)
  }

  test("Parse BOL and EOL") {
    val pattern = "^hello$"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[Concat])
    assert(clue(parsedTree.children.head).isInstanceOf[BOL])
    assert(clue(parsedTree.children.last).isInstanceOf[EOL])

    val buildPattern: String = parsedTree.build
    assertEquals(buildPattern, pattern)
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
    val buildPattern: String = parsedTree.build

    assertEquals(
      parsedTree.children filter {
        case Character('a', _) => true
        case _                 => false
      } map (_.location.start.line),
      0 to 5
    )
    assertEquals(buildPattern, pattern)
  }

  test("Parse character class with characters") {
    val pattern = "[abc]"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[CharacterClass])

    val buildPattern: String = parsedTree.build
    assertEquals(buildPattern, pattern)
  }

  test("Parse negative character class with characters") {
    val pattern = "[^abc]"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[CharacterClass])

    val buildPattern: String = parsedTree.build
    assertEquals(buildPattern, pattern)
  }

  test("Parse character class with ranges") {
    val pattern = "[a-zA-Z0-9]"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[CharacterClass])
    assert(clue(parsedTree.children) forall (_.isInstanceOf[Range]))

    val buildPattern: String = parsedTree.build
    assertEquals(buildPattern, pattern)
  }

  test("Parse character class with nested character classes") {
    val pattern = "[[a-z][^A-Z0-9][01234]]"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree).isInstanceOf[CharacterClass])
    assert(clue(parsedTree.children) forall (_.isInstanceOf[CharacterClass]))

    val buildPattern: String = parsedTree.build
    assertEquals(buildPattern, pattern)
  }

  test("Parse escape characters") {
    val pattern = """\\\t\n\r\f"""
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children) forall (_.isInstanceOf[MetaChar]))

    val buildPattern: String = parsedTree.build
    assertEquals(buildPattern, pattern)
  }

  test("Parse hexadecimal characters") {
    val pattern = "\\x20\\u0020\\x{000020}"
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children) forall (_.isInstanceOf[MetaChar]))

    val buildPattern: String = parsedTree.build
    assertEquals(buildPattern, pattern)
  }

  test("Parse octal characters") {
    val pattern = """\01\012\0123"""
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children) forall (_.isInstanceOf[MetaChar]))

    val buildPattern: String = parsedTree.build
    assertEquals(buildPattern, pattern)
  }

  test("Parse predefined character class") {
    val pattern = """.\w\W\s\S\d\D"""
    val parsedTree = Parser.parseOrError(pattern)
    assert(clue(parsedTree.children.head).isInstanceOf[Any])
    assert(clue(parsedTree.children.tail) forall (_.isInstanceOf[PredefinedCharClass]))

    val buildPattern: String = parsedTree.build
    assertEquals(buildPattern, pattern)
  }
}
