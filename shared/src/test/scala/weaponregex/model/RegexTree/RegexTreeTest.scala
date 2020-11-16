package weaponregex.model.RegexTree

import weaponregex.model._
import weaponregex.model.RegexTree._

class RegexTreeTest extends munit.FunSuite {
  var locStub: Location = Location(Position(0, 0), Position(0, 1))

  test("Character build") {
    val node1 = Character('a', locStub)
    assertEquals("a", node1.build)

    val node2 = Character('b', locStub)
    assertEquals("b", node2.build)
  }

  test("RegexTree build") {
    val pattern: String = """^\w+@[a-zA-Z_]+?\.[a-zA-Z]{2,3}$"""
    val loc = Location(Position(0, 0), Position(0, 1))
    val tree: RegexTree = Concat(
      Seq(
        BOL(loc),
        OneOrMore(
          PredefinedCharClass("w", isPositive = true, loc),
          loc
        ),
        Character('@', loc),
        OneOrMore(
          CharacterClass(
            Seq(
              Range(Character('a', loc), Character('z', loc), loc),
              Range(Character('A', loc), Character('Z', loc), loc),
              Character('_', loc)
            ),
            loc
          ),
          loc,
          isReluctant = true
        ),
        QuoteChar('.', loc),
        Quantifier(
          CharacterClass(
            Seq(
              Range(Character('a', loc), Character('z', loc), loc),
              Range(Character('A', loc), Character('Z', loc), loc)
            ),
            loc
          ),
          min = 2,
          hasComma = true,
          max = 3,
          loc
        ),
        EOL(loc)
      ),
      loc
    )
    val buildResult = tree.build
    assertEquals(pattern, buildResult)
  }
}
