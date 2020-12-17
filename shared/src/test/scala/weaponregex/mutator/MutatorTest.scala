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

    val expected: Seq[String] = Seq(
      "abc^def^",
      "^abcdef^",
      "^abc^def"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Remove EOL") {
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

  test("Change BOL to BOI") {
    val pattern = "^abc^def^"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(BOL2BOI))
    assertEquals(clue(mutants).length, 3)

    val expected: Seq[String] = Seq(
      """\Aabc^def^""",
      """^abc\Adef^""",
      """^abc^def\A"""
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Change EOL to EOI") {
    val pattern = "$abc$def$"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(EOL2EOI))
    assertEquals(clue(mutants).length, 3)

    val expected: Seq[String] = Seq(
      """\zabc$def$""",
      """$abc\zdef$""",
      """$abc$def\z"""
    ).sorted
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

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(CharClassChildRemoval))
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

  test("Character Class Modify Range with lower case letters") {
    val pattern = "[b-y][a-y][b-z][a-z][b-b][a-a][z-z]"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(CharClassRangeModification))
    assertEquals(clue(mutants).length, 16)

    val expected: Seq[String] = Seq(
      // [b-y] -> [a-y] or [c-y] or [b-z] or [b-x]
      "[a-y][a-y][b-z][a-z][b-b][a-a][z-z]",
      "[c-y][a-y][b-z][a-z][b-b][a-a][z-z]",
      "[b-z][a-y][b-z][a-z][b-b][a-a][z-z]",
      "[b-x][a-y][b-z][a-z][b-b][a-a][z-z]",
      // [a-y] -> [b-y] or [a-z] or [a-x]
      "[b-y][b-y][b-z][a-z][b-b][a-a][z-z]",
      "[b-y][a-z][b-z][a-z][b-b][a-a][z-z]",
      "[b-y][a-x][b-z][a-z][b-b][a-a][z-z]",
      // [b-z] -> [a-z] or [c-z] or [b-y]
      "[b-y][a-y][a-z][a-z][b-b][a-a][z-z]",
      "[b-y][a-y][c-z][a-z][b-b][a-a][z-z]",
      "[b-y][a-y][b-y][a-z][b-b][a-a][z-z]",
      // [a-z] -> [b-z] or [a-y]
      "[b-y][a-y][b-z][b-z][b-b][a-a][z-z]",
      "[b-y][a-y][b-z][a-y][b-b][a-a][z-z]",
      // [b-b] -> [a-b] or [b-c]
      "[b-y][a-y][b-z][a-z][a-b][a-a][z-z]",
      "[b-y][a-y][b-z][a-z][b-c][a-a][z-z]",
      // [a-a] -> [a-b]
      "[b-y][a-y][b-z][a-z][b-b][a-b][z-z]",
      // [z-z] -> [y-z]
      "[b-y][a-y][b-z][a-z][b-b][a-a][y-z]"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Character Class Modify Range with upper case letters") {
    val pattern = "[B-Y][A-Y][B-Z][A-Z][B-B][A-A][Z-Z]"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(CharClassRangeModification))
    assertEquals(clue(mutants).length, 16)

    val expected: Seq[String] = Seq(
      // [B-Y] -> [A-Y] OR [C-Y] OR [B-Z] OR [B-X]
      "[A-Y][A-Y][B-Z][A-Z][B-B][A-A][Z-Z]",
      "[C-Y][A-Y][B-Z][A-Z][B-B][A-A][Z-Z]",
      "[B-Z][A-Y][B-Z][A-Z][B-B][A-A][Z-Z]",
      "[B-X][A-Y][B-Z][A-Z][B-B][A-A][Z-Z]",
      // [A-Y] -> [B-Y] OR [A-Z] OR [A-X]
      "[B-Y][B-Y][B-Z][A-Z][B-B][A-A][Z-Z]",
      "[B-Y][A-Z][B-Z][A-Z][B-B][A-A][Z-Z]",
      "[B-Y][A-X][B-Z][A-Z][B-B][A-A][Z-Z]",
      // [B-Z] -> [A-Z] OR [C-Z] OR [B-Y]
      "[B-Y][A-Y][A-Z][A-Z][B-B][A-A][Z-Z]",
      "[B-Y][A-Y][C-Z][A-Z][B-B][A-A][Z-Z]",
      "[B-Y][A-Y][B-Y][A-Z][B-B][A-A][Z-Z]",
      // [A-Z] -> [B-Z] OR [A-Y]
      "[B-Y][A-Y][B-Z][B-Z][B-B][A-A][Z-Z]",
      "[B-Y][A-Y][B-Z][A-Y][B-B][A-A][Z-Z]",
      // [B-B] -> [A-B] OR [B-C]
      "[B-Y][A-Y][B-Z][A-Z][A-B][A-A][Z-Z]",
      "[B-Y][A-Y][B-Z][A-Z][B-C][A-A][Z-Z]",
      // [A-A] -> [A-B]
      "[B-Y][A-Y][B-Z][A-Z][B-B][A-B][Z-Z]",
      // [Z-Z] -> [Y-Z]
      "[B-Y][A-Y][B-Z][A-Z][B-B][A-A][Y-Z]"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Character Class Modify Range with numbers") {
    val pattern = "[1-8][0-8][1-9][0-9][1-1][0-0][9-9]"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(CharClassRangeModification))
    assertEquals(clue(mutants).length, 16)

    val expected: Seq[String] = Seq(
      // [1-8] -> [0-8] OR [2-8] OR [1-9] OR [1-7]
      "[0-8][0-8][1-9][0-9][1-1][0-0][9-9]",
      "[2-8][0-8][1-9][0-9][1-1][0-0][9-9]",
      "[1-9][0-8][1-9][0-9][1-1][0-0][9-9]",
      "[1-7][0-8][1-9][0-9][1-1][0-0][9-9]",
      // [0-8] -> [1-8] OR [0-9] OR [0-7]
      "[1-8][1-8][1-9][0-9][1-1][0-0][9-9]",
      "[1-8][0-9][1-9][0-9][1-1][0-0][9-9]",
      "[1-8][0-7][1-9][0-9][1-1][0-0][9-9]",
      // [1-9] -> [0-9] OR [2-9] OR [1-8]
      "[1-8][0-8][0-9][0-9][1-1][0-0][9-9]",
      "[1-8][0-8][2-9][0-9][1-1][0-0][9-9]",
      "[1-8][0-8][1-8][0-9][1-1][0-0][9-9]",
      // [0-9] -> [1-9] OR [0-8]
      "[1-8][0-8][1-9][1-9][1-1][0-0][9-9]",
      "[1-8][0-8][1-9][0-8][1-1][0-0][9-9]",
      // [1-1] -> [0-1] OR [1-2]
      "[1-8][0-8][1-9][0-9][0-1][0-0][9-9]",
      "[1-8][0-8][1-9][0-9][1-2][0-0][9-9]",
      // [0-0] -> [0-1]
      "[1-8][0-8][1-9][0-9][1-1][0-1][9-9]",
      // [9-9] -> [8-9]
      "[1-8][0-8][1-9][0-9][1-1][0-0][8-9]"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Remove greedy quantifier") {
    val pattern = "a?b*c+d{1}e{1,}f{1,2}"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(QuantifierRemoval))
    assertEquals(clue(mutants).length, 6)

    val expected: Seq[String] = Seq(
      "ab*c+d{1}e{1,}f{1,2}",
      "a?bc+d{1}e{1,}f{1,2}",
      "a?b*cd{1}e{1,}f{1,2}",
      "a?b*c+de{1,}f{1,2}",
      "a?b*c+d{1}ef{1,2}",
      "a?b*c+d{1}e{1,}f"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Remove reluctant quantifier") {
    val pattern = "a??b*?c+?d{1}?e{1,}?f{1,2}?"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(QuantifierRemoval))
    assertEquals(clue(mutants).length, 6)

    val expected: Seq[String] = Seq(
      "ab*?c+?d{1}?e{1,}?f{1,2}?",
      "a??bc+?d{1}?e{1,}?f{1,2}?",
      "a??b*?cd{1}?e{1,}?f{1,2}?",
      "a??b*?c+?de{1,}?f{1,2}?",
      "a??b*?c+?d{1}?ef{1,2}?",
      "a??b*?c+?d{1}?e{1,}?f"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Remove possessive quantifier") {
    val pattern = "a?+b*+c++d{1}+e{1,}+f{1,2}+"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(QuantifierRemoval))
    assertEquals(clue(mutants).length, 6)

    val expected: Seq[String] = Seq(
      "ab*+c++d{1}+e{1,}+f{1,2}+",
      "a?+bc++d{1}+e{1,}+f{1,2}+",
      "a?+b*+cd{1}+e{1,}+f{1,2}+",
      "a?+b*+c++de{1,}+f{1,2}+",
      "a?+b*+c++d{1}+ef{1,2}+",
      "a?+b*+c++d{1}+e{1,}+f"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Change quantifier {n}") {
    val pattern = "a{1}"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(QuantifierNChange))
    assertEquals(clue(mutants).length, 2)

    val expected: Seq[String] = Seq(
      "a{0,1}",
      "a{1,}"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Modify quantifier {n,}") {
    val pattern = "a{0,}b{1,}"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(QuantifierNOrMoreModification))
    assertEquals(clue(mutants).length, 3)

    val expected: Seq[String] = Seq(
      "a{1,}b{1,}",
      "a{0,}b{0,}",
      "a{0,}b{2,}"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Change quantifier {n,}") {
    val pattern = "a{1,}"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(QuantifierNOrMoreChange))
    assertEquals(clue(mutants).length, 1)

    val expected: Seq[String] = Seq("a{1}").sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Modify quantifier {n,m}") {
    val pattern = "a{0,0}b{0,1}c{1,2}"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(QuantifierNMModification))
    assertEquals(clue(mutants).length, 8)

    val expected: Seq[String] = Seq(
      "a{0,1}b{0,1}c{1,2}",
      "a{0,0}b{1,1}c{1,2}",
      "a{0,0}b{0,0}c{1,2}",
      "a{0,0}b{0,2}c{1,2}",
      "a{0,0}b{0,1}c{0,2}",
      "a{0,0}b{0,1}c{2,2}",
      "a{0,0}b{0,1}c{1,1}",
      "a{0,0}b{0,1}c{1,3}"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Modify short quantifier") {
    val pattern = "a?b*c+"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(QuantifierShortModification))
    assertEquals(clue(mutants).length, 6)

    val expected: Seq[String] = Seq(
      "a{1,1}b*c+",
      "a{0,0}b*c+",
      "a{0,2}b*c+",
      "a?b{1,}c+",
      "a?b*c{0,}",
      "a?b*c{2,}"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Change short quantifier") {
    val pattern = "a*b+"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(QuantifierShortChange))
    assertEquals(clue(mutants).length, 2)

    val expected: Seq[String] = Seq(
      "a{0}b+",
      "a*b{1}"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Add reluctant to greedy quantifier") {
    val pattern = "a?b*c+d{1}e{1,}f{1,2}"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(QuantifierReluctantAddition))
    assertEquals(clue(mutants).length, 6)

    val expected: Seq[String] = Seq(
      "a??b*c+d{1}e{1,}f{1,2}",
      "a?b*?c+d{1}e{1,}f{1,2}",
      "a?b*c+?d{1}e{1,}f{1,2}",
      "a?b*c+d{1}?e{1,}f{1,2}",
      "a?b*c+d{1}e{1,}?f{1,2}",
      "a?b*c+d{1}e{1,}f{1,2}?"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }
}
