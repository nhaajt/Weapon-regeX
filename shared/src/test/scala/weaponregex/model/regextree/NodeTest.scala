package weaponregex.model.regextree

import weaponregex.model._
import weaponregex.model.regextree._

class NodeTest extends munit.FunSuite {
  var locStub: Location = Location(Position(0, 0), Position(0, 1))
  var leafStubA: Character = Character('A', locStub)
  var leafStubB: Character = Character('B', locStub)
  var leafStubC: Character = Character('C', locStub)

  test("CharacterClass build") {
    var node1 = CharacterClass(Seq(leafStubA, leafStubB, leafStubC), locStub)
    assertEquals(node1.build, "[ABC]")

    var node2 = CharacterClass(Seq(leafStubA, leafStubB, leafStubC), locStub, false)
    assertEquals(node2.build, "[^ABC]")
  }

  test("Range build") {
    var node1 = Range(leafStubA, leafStubC, locStub)
    assertEquals(node1.build, "A-C")
  }

  test("Group build") {
    var node1 = Group(leafStubA, true, locStub)
    assertEquals(node1.build, "(A)")

    var node2 = Group(leafStubA, false, locStub)
    assertEquals(node2.build, "(?:A)")
  }

  test("NamedGroup build") {
    var node1 = NamedGroup(leafStubA, "name", locStub)
    assertEquals(node1.build, "(?<name>A)")
  }

  test("FlagNCGroup build") {
    var charSeq = Seq(leafStubA, leafStubB, leafStubC)
    var node1 =
      FlagNCGroup(FlagToggle(Flags(charSeq, locStub), true, Flags(charSeq, locStub), locStub), leafStubA, locStub)
    assertEquals(node1.build, "(?ABC-ABC:A)")

    var node2 =
      FlagNCGroup(FlagToggle(Flags(Seq(), locStub), true, Flags(charSeq, locStub), locStub), leafStubA, locStub)
    assertEquals(node2.build, "(?-ABC:A)")

    var node3 =
      FlagNCGroup(FlagToggle(Flags(charSeq, locStub), false, Flags(Seq(), locStub), locStub), leafStubA, locStub)
    assertEquals(node3.build, "(?ABC:A)")
  }

  test("FlagGroup build") {
    var charSeq = Seq(leafStubA, leafStubB, leafStubC)
    var node1 =
      FlagGroup(FlagToggle(Flags(charSeq, locStub), true, Flags(charSeq, locStub), locStub), locStub)
    assertEquals(node1.build, "(?ABC-ABC)")

    var node2 =
      FlagGroup(FlagToggle(Flags(Seq(), locStub), true, Flags(charSeq, locStub), locStub), locStub)
    assertEquals(node2.build, "(?-ABC)")

    var node3 =
      FlagGroup(FlagToggle(Flags(charSeq, locStub), false, Flags(Seq(), locStub), locStub), locStub)
    assertEquals(node3.build, "(?ABC)")
  }

  test("Lookaround build") {
    var node1 = Lookaround(leafStubA, false, false, locStub)
    assertEquals(node1.build, "(?<!A)")

    var node2 = Lookaround(leafStubA, true, false, locStub)
    assertEquals(node2.build, "(?<=A)")

    var node3 = Lookaround(leafStubA, false, true, locStub)
    assertEquals(node3.build, "(?!A)")

    var node4 = Lookaround(leafStubA, true, true, locStub)
    assertEquals(node4.build, "(?=A)")
  }

  test("INCGroup build") {
    var node1 = INCGroup(leafStubA, locStub)
    assertEquals(node1.build, "(?>A)")
  }

  test("Quantifier build") {
    //TODO test reworked stuff

    var node1 = Quantifier(leafStubA, 1, true, 3, locStub)
    assertEquals(node1.build, "A{1,3}")

    var node2 = Quantifier(leafStubA, 1, true, 3, locStub, true, false)
    assertEquals(node2.build, "A{1,3}?")

    var node3 = Quantifier(leafStubA, 1, true, 3, locStub, false, true)
    assertEquals(node3.build, "A{1,3}+")
  }

  test("ZeroOrOne build") {
    var node1 = ZeroOrOne(leafStubA, locStub)
    assertEquals(node1.build, "A?")

    var node2 = ZeroOrOne(leafStubA, locStub, true, false)
    assertEquals(node2.build, "A??")

    var node3 = ZeroOrOne(leafStubA, locStub, false, true)
    assertEquals(node3.build, "A?+")
  }

  test("ZeroOrMore build") {
    var node1 = ZeroOrMore(leafStubA, locStub)
    assertEquals(node1.build, "A*")

    var node2 = ZeroOrMore(leafStubA, locStub, true, false)
    assertEquals(node2.build, "A*?")

    var node3 = ZeroOrMore(leafStubA, locStub, false, true)
    assertEquals(node3.build, "A*+")
  }

  test("OneOrMore build") {
    var node1 = OneOrMore(leafStubA, locStub)
    assertEquals(node1.build, "A+")

    var node2 = OneOrMore(leafStubA, locStub, true, false)
    assertEquals(node2.build, "A+?")

    var node3 = OneOrMore(leafStubA, locStub, false, true)
    assertEquals(node3.build, "A++")
  }

  test("Concat build") {
    var node1 = Concat(Seq(leafStubA, leafStubB), locStub)
    assertEquals(node1.build, "AB")

    var node2 = Concat(Seq(leafStubA, leafStubB, leafStubC), locStub)
    assertEquals(node2.build, "ABC")
  }

  test("Or build") {
    var node1 = Or(Seq(leafStubA, leafStubB), locStub)
    assertEquals(node1.build, "A|B")

    var node2 = Or(Seq(leafStubA, leafStubB, leafStubC), locStub)
    assertEquals(node2.build, "A|B|C")
  }

}
