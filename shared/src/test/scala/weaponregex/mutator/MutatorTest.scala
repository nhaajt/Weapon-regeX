package weaponregex.mutator

import weaponregex.run.Parser
import weaponregex.run.TreeMutator._
import weaponregex.model.mutation._

class MutatorTest extends munit.FunSuite {
  test("Remove BOL") {
    val pattern = "^abc^def^"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(BOLRemoval))
    assertEquals(clue(mutants).length, 3)

    val expected: Seq[String] = Seq("abc^def^", "^abcdef^", "^abc^def").sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Remove EOL") {
    val pattern = "$abc$def$"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(EOLRemoval))
    assertEquals(clue(mutants).length, 3)

    val expected: Seq[String] = Seq("abc$def$", "$abcdef$", "$abc$def").sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Negate Predefined Character Class") {
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

  test("Nullify Predefined Character Class") {
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

  test("Predefined Character Class to Any Char") {
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

  test("Character Class Negation") {
    val pattern = "[[abc][^abc]]"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(CharClassNegation))
    assertEquals(clue(mutants).length, 3)

    val expected: Seq[String] = Seq(
      "[^[abc][^abc]]",
      "[[^abc][^abc]]",
      "[[abc][abc]]"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Character Class Remove Child") {
    val pattern = "[ab0-9[A-Z][cd]]"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(CharClassRemoveChild))
    assertEquals(clue(mutants).length, 7)

    val expected: Seq[String] = Seq(
      "[b0-9[A-Z][cd]]",
      "[a0-9[A-Z][cd]]",
      "[ab[A-Z][cd]]",
      "[ab0-9[cd]]",
      "[ab0-9[A-Z]]",
      "[ab0-9[A-Z][d]]",
      "[ab0-9[A-Z][c]]"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Character Class Remove Child") {
    val pattern = "[abc[0-9]]"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(CharClassAnyChar))
    assertEquals(clue(mutants).length, 2)

    val expected: Seq[String] = Seq(
      """[\w\W]""",
      """[abc[\w\W]]"""
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }
}
