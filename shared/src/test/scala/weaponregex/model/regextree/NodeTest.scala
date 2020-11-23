package weaponregex.model.regextree

import weaponregex.model._

class NodeTest extends munit.FunSuite {
  val locStub: Location = Location(Position(0, 0), Position(0, 1))
  val leafStubA: Character = Character('A', locStub)
  val leafStubB: Character = Character('B', locStub)
  val leafStubC: Character = Character('C', locStub)

  test("CharacterClass build") {
    val node1 = CharacterClass(Seq(leafStubA, leafStubB, leafStubC), locStub)
    assertEquals(node1.build, "[ABC]")

    val node2 = CharacterClass(Seq(leafStubA, leafStubB, leafStubC), locStub, isPositive = false)
    assertEquals(node2.build, "[^ABC]")
  }

  test("Range build") {
    val node1 = Range(leafStubA, leafStubC, locStub)
    assertEquals(node1.build, "A-C")
  }

  test("Group build") {
    val node1 = Group(leafStubA, isCapturing = true, locStub)
    assertEquals(node1.build, "(A)")

    val node2 = Group(leafStubA, isCapturing = false, locStub)
    assertEquals(node2.build, "(?:A)")
  }

  test("NamedGroup build") {
    val node1 = NamedGroup(leafStubA, "name", locStub)
    assertEquals(node1.build, "(?<name>A)")
  }

  test("FlagNCGroup build") {
    val charSeq = Seq(leafStubA, leafStubB, leafStubC)
    val node1 =
      FlagNCGroup(
        FlagToggle(Flags(charSeq, locStub), hasDash = true, Flags(charSeq, locStub), locStub),
        leafStubA,
        locStub
      )
    assertEquals(node1.build, "(?ABC-ABC:A)")

    val node2 =
      FlagNCGroup(
        FlagToggle(Flags(Seq(), locStub), hasDash = true, Flags(charSeq, locStub), locStub),
        leafStubA,
        locStub
      )
    assertEquals(node2.build, "(?-ABC:A)")

    val node3 =
      FlagNCGroup(
        FlagToggle(Flags(charSeq, locStub), hasDash = false, Flags(Seq(), locStub), locStub),
        leafStubA,
        locStub
      )
    assertEquals(node3.build, "(?ABC:A)")
  }

  test("FlagGroup build") {
    val charSeq = Seq(leafStubA, leafStubB, leafStubC)
    val node1 =
      FlagGroup(FlagToggle(Flags(charSeq, locStub), hasDash = true, Flags(charSeq, locStub), locStub), locStub)
    assertEquals(node1.build, "(?ABC-ABC)")

    val node2 =
      FlagGroup(FlagToggle(Flags(Seq(), locStub), hasDash = true, Flags(charSeq, locStub), locStub), locStub)
    assertEquals(node2.build, "(?-ABC)")

    val node3 =
      FlagGroup(FlagToggle(Flags(charSeq, locStub), hasDash = false, Flags(Seq(), locStub), locStub), locStub)
    assertEquals(node3.build, "(?ABC)")
  }

  test("Lookaround build") {
    val node1 = Lookaround(leafStubA, isPositive = false, isLookahead = false, locStub)
    assertEquals(node1.build, "(?<!A)")

    val node2 = Lookaround(leafStubA, isPositive = true, isLookahead = false, locStub)
    assertEquals(node2.build, "(?<=A)")

    val node3 = Lookaround(leafStubA, isPositive = false, isLookahead = true, locStub)
    assertEquals(node3.build, "(?!A)")

    val node4 = Lookaround(leafStubA, isPositive = true, isLookahead = true, locStub)
    assertEquals(node4.build, "(?=A)")
  }

  test("INCGroup build") {
    val node1 = INCGroup(leafStubA, locStub)
    assertEquals(node1.build, "(?>A)")
  }

  test("Quantifier build") {
    //TODO test reworked stuff

    val node1 = Quantifier(leafStubA, 1, hasComma = true, 3, locStub)
    assertEquals(node1.build, "A{1,3}")

    val node2 = Quantifier(leafStubA, 1, hasComma = true, 3, locStub, isReluctant = true, isPossessive = false)
    assertEquals(node2.build, "A{1,3}?")

    val node3 = Quantifier(leafStubA, 1, hasComma = true, 3, locStub, isReluctant = false, isPossessive = true)
    assertEquals(node3.build, "A{1,3}+")
  }

  test("ZeroOrOne build") {
    val node1 = ZeroOrOne(leafStubA, locStub)
    assertEquals(node1.build, "A?")

    val node2 = ZeroOrOne(leafStubA, locStub, isReluctant = true, isPossessive = false)
    assertEquals(node2.build, "A??")

    val node3 = ZeroOrOne(leafStubA, locStub, isReluctant = false, isPossessive = true)
    assertEquals(node3.build, "A?+")
  }

  test("ZeroOrMore build") {
    val node1 = ZeroOrMore(leafStubA, locStub)
    assertEquals(node1.build, "A*")

    val node2 = ZeroOrMore(leafStubA, locStub, isReluctant = true, isPossessive = false)
    assertEquals(node2.build, "A*?")

    val node3 = ZeroOrMore(leafStubA, locStub, isReluctant = false, isPossessive = true)
    assertEquals(node3.build, "A*+")
  }

  test("OneOrMore build") {
    val node1 = OneOrMore(leafStubA, locStub)
    assertEquals(node1.build, "A+")

    val node2 = OneOrMore(leafStubA, locStub, isReluctant = true, isPossessive = false)
    assertEquals(node2.build, "A+?")

    val node3 = OneOrMore(leafStubA, locStub, isReluctant = false, isPossessive = true)
    assertEquals(node3.build, "A++")
  }

  test("Concat build") {
    val node1 = Concat(Seq(leafStubA, leafStubB), locStub)
    assertEquals(node1.build, "AB")

    val node2 = Concat(Seq(leafStubA, leafStubB, leafStubC), locStub)
    assertEquals(node2.build, "ABC")
  }

  test("Or build") {
    val node1 = Or(Seq(leafStubA, leafStubB), locStub)
    assertEquals(node1.build, "A|B")

    val node2 = Or(Seq(leafStubA, leafStubB, leafStubC), locStub)
    assertEquals(node2.build, "A|B|C")
  }

}
