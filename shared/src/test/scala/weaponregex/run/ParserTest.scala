package weaponregex.run

import weaponregex.model.regextree._

class ParserTest extends munit.FunSuite {
  test("Parse concat of characters") {
    val pattern = "hello"
    val parsedTree = Parser.parseOrError(pattern)
    val buildPattern: String = parsedTree.build
    assertEquals(pattern, buildPattern)
  }

  test("Parse or of characters") {
    val pattern = "h|e|l|l|o"
    val parsedTree = Parser.parseOrError(pattern)
    val buildPattern: String = parsedTree.build
    assertEquals(pattern, buildPattern)
  }

  test("Parse BOL and EOL") {
    val pattern = "^hello$"
    val parsedTree = Parser.parseOrError(pattern)
    val buildPattern: String = parsedTree.build
    assertEquals(pattern, buildPattern)
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
    assertEquals(pattern, buildPattern)
  }

  test("Parse character class with characters") {
    val pattern = "[abc]"
    val parsedTree = Parser.parseOrError(pattern)
    val buildPattern: String = parsedTree.build
    assertEquals(pattern, buildPattern)
  }

  test("Parse negative character class with characters") {
    val pattern = "[^abc]"
    val parsedTree = Parser.parseOrError(pattern)
    val buildPattern: String = parsedTree.build
    assertEquals(pattern, buildPattern)
  }

  test("Parse character class with ranges") {
    val pattern = "[a-zA-Z0-9]"
    val parsedTree = Parser.parseOrError(pattern)
    val buildPattern: String = parsedTree.build
    assertEquals(pattern, buildPattern)
  }

  test("Parse character class with nested character classes") {
    val pattern = "[[a-z][^A-Z0-9][01234]]"
    val parsedTree = Parser.parseOrError(pattern)
    val buildPattern: String = parsedTree.build
    assertEquals(pattern, buildPattern)
  }
}
