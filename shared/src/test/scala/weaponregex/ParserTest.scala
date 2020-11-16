package weaponregex

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
      """0
        |1
        |2
        |3
        |4
        |5""".stripMargin
    val parsedTree = Parser.parseOrError(pattern)
    val buildPattern: String = parsedTree.build

    assertEquals(
      parsedTree.children filter {
        case Character(c, _) if c.isDigit => true
        case _                            => false
      } map (_.location.start.line),
      0 to 5
    )

//    assertEquals(parsedTree.children.head.location.start.line, 0)
//    assertEquals(parsedTree.children.last.location.start.line, 3)
    assertEquals(pattern, buildPattern)
  }
}
