package weaponregex.mutator

import weaponregex.parser.Parser
import TreeMutator._
import weaponregex.model.mutation.Mutant

class PredefCharClassMutatorTest extends munit.FunSuite {
  test("Negates Predefined Character Class") {
    val pattern = """\w\W\d\D\s\S"""
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(PredefCharClassNegation))
    assertEquals(clue(mutants).length, 6)

    val expected: Seq[String] = Seq(
      """\W\W\d\D\s\S""",
      """\w\w\d\D\s\S""",
      """\w\W\D\D\s\S""",
      """\w\W\d\d\s\S""",
      """\w\W\d\D\S\S""",
      """\w\W\d\D\s\s"""
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Does not mutate (negate) similar characters") {
    val pattern = "wWdDsS"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(PredefCharClassNegation))
    assertEquals(clue(mutants).length, 0)
  }

  test("Nullifies Predefined Character Class") {
    val pattern = """\w\W\d\D\s\S"""
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(PredefCharClassNullification))
    assertEquals(clue(mutants).length, 6)

    val expected: Seq[String] = Seq(
      """w\W\d\D\s\S""",
      """\wW\d\D\s\S""",
      """\w\Wd\D\s\S""",
      """\w\W\dD\s\S""",
      """\w\W\d\Ds\S""",
      """\w\W\d\D\sS"""
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Does not mutate (nullify) similar characters") {
    val pattern = "wWdDsS"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(PredefCharClassNullification))
    assertEquals(clue(mutants).length, 0)
  }

  test("Changes Predefined Character Class to Any Char") {
    val pattern = """\w\W\d\D\s\S"""
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(PredefCharClassAnyChar))
    assertEquals(clue(mutants).length, 6)

    val expected: Seq[String] = Seq(
      """[\w\W]\W\d\D\s\S""",
      """\w[\W\w]\d\D\s\S""",
      """\w\W[\d\D]\D\s\S""",
      """\w\W\d[\D\d]\s\S""",
      """\w\W\d\D[\s\S]\S""",
      """\w\W\d\D\s[\S\s]"""
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Does not mutate (change) similar characters") {
    val pattern = "wWdDsS"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(PredefCharClassAnyChar))
    assertEquals(clue(mutants).length, 0)
  }
}
