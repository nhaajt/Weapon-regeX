package weaponregex.mutator

import weaponregex.parser.Parser
import TreeMutator._
import weaponregex.model.mutation.Mutant

class BoundaryMutatorTest extends munit.FunSuite {
  test("Removes BOL") {
    val pattern = "^abc^def^ghi\\^"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(BOLRemoval))
    assertEquals(clue(mutants).length, 3)

    val expected: Seq[String] = Seq(
      "abc^def^ghi\\^",
      "^abcdef^ghi\\^",
      "^abc^defghi\\^"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Does not remove escaped BOL") {
    val pattern = "\\^abc"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(BOLRemoval))
    assertEquals(clue(mutants).length, 0)
  }

  test("Removes EOL") {
    val pattern = "$abc$def$"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(EOLRemoval))
    assertEquals(clue(mutants).length, 3)

    val expected: Seq[String] = Seq(
      "abc$def$",
      "$abcdef$",
      "$abc$def"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Does not remove escaped EOL") {
    val pattern = "abc\\$"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(EOLRemoval))
    assertEquals(clue(mutants).length, 0)
  }

  test("Changes BOL to BOI") {
    val pattern = "^abc^def^ghi\\^"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(BOL2BOI))
    assertEquals(clue(mutants).length, 3)

    val expected: Seq[String] = Seq(
      """\Aabc^def^ghi\^""",
      """^abc\Adef^ghi\^""",
      """^abc^def\Aghi\^"""
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Does not change escaped BOL") {
    val pattern = "\\^abc"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(BOL2BOI))
    assertEquals(clue(mutants).length, 0)
  }

  test("Changes EOL to EOI") {
    val pattern = "$abc$def$ghi\\$"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(EOL2EOI))
    assertEquals(clue(mutants).length, 3)

    val expected: Seq[String] = Seq(
      """\zabc$def$ghi\$""",
      """$abc\zdef$ghi\$""",
      """$abc$def\zghi\$"""
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Does not change escaped EOL") {
    val pattern = "abc\\$"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(EOL2EOI))
    assertEquals(clue(mutants).length, 0)
  }
}
