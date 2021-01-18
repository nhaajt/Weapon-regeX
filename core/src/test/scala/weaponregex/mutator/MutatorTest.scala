package weaponregex.mutator

import weaponregex.parser.Parser
import TreeMutator._
import weaponregex.model.mutation._

class MutatorTest extends munit.FunSuite {
  test("Mutator name is non-empty") {
    BuiltinMutators.all foreach (mutator => assert(clue(mutator).name.nonEmpty))
  }

  test("Mutator description is non-empty") {
    BuiltinMutators.all foreach (mutator => assert(clue(mutator).description.nonEmpty))
  }

  test("Remove BOL") {
    val pattern = "^abc^def^"
    val parsedTree = Parser(pattern).get

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
    val parsedTree = Parser(pattern).get

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
    val parsedTree = Parser(pattern).get

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
    val parsedTree = Parser(pattern).get

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
    val parsedTree = Parser(pattern).get

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
    val parsedTree = Parser(pattern).get

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
    val parsedTree = Parser(pattern).get

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
    val parsedTree = Parser(pattern).get

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
    val parsedTree = Parser(pattern).get

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

  test("Character Class to any char") {
    val pattern = "[abc[0-9]]"
    val parsedTree = Parser(pattern).get

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(CharClassAnyChar))
    assertEquals(clue(mutants).length, 2)

    val expected: Seq[String] = Seq(
      """[\w\W]""",
      """[abc[\w\W]]"""
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Character Class Modify Range [b-y][B-Y][1-8]") {
    val pattern = "[b-y][B-Y][1-8]"
    val parsedTree = Parser(pattern).get

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(CharClassRangeModification))
    assertEquals(clue(mutants).length, 12)

    val expected: Seq[String] = Seq(
      // [b-y] -> [a-y] or [c-y] or [b-z] or [b-x]
      "[a-y][B-Y][1-8]",
      "[c-y][B-Y][1-8]",
      "[b-z][B-Y][1-8]",
      "[b-x][B-Y][1-8]",
      // [B-Y] -> [A-Y] OR [C-Y] OR [B-Z] OR [B-X]
      "[b-y][A-Y][1-8]",
      "[b-y][C-Y][1-8]",
      "[b-y][B-Z][1-8]",
      "[b-y][B-X][1-8]",
      // [1-8] -> [0-8] OR [2-8] OR [1-9] OR [1-7]
      "[b-y][B-Y][0-8]",
      "[b-y][B-Y][2-8]",
      "[b-y][B-Y][1-9]",
      "[b-y][B-Y][1-7]"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Character Class Modify Range [a-y][A-Y][0-8]") {
    val pattern = "[a-y][A-Y][0-8]"
    val parsedTree = Parser(pattern).get

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(CharClassRangeModification))
    assertEquals(clue(mutants).length, 9)

    val expected: Seq[String] = Seq(
      // [a-y] -> [b-y] or [a-z] or [a-x]
      "[b-y][A-Y][0-8]",
      "[a-z][A-Y][0-8]",
      "[a-x][A-Y][0-8]",
      // [A-Y] -> [B-Y] OR [A-Z] OR [A-X]
      "[a-y][B-Y][0-8]",
      "[a-y][A-Z][0-8]",
      "[a-y][A-X][0-8]",
      // [0-8] -> [1-8] OR [0-9] OR [0-7]
      "[a-y][A-Y][1-8]",
      "[a-y][A-Y][0-9]",
      "[a-y][A-Y][0-7]"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Character Class Modify Range [b-z][B-Z][1-9]") {
    val pattern = "[b-z][B-Z][1-9]"
    val parsedTree = Parser(pattern).get

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(CharClassRangeModification))
    assertEquals(clue(mutants).length, 9)

    val expected: Seq[String] = Seq(
      // [b-z] -> [a-z] or [c-z] or [b-y]
      "[a-z][B-Z][1-9]",
      "[c-z][B-Z][1-9]",
      "[b-y][B-Z][1-9]",
      // [B-Z] -> [A-Z] OR [C-Z] OR [B-Y]
      "[b-z][A-Z][1-9]",
      "[b-z][C-Z][1-9]",
      "[b-z][B-Y][1-9]",
      // [1-9] -> [0-9] OR [2-9] OR [1-8]
      "[b-z][B-Z][0-9]",
      "[b-z][B-Z][2-9]",
      "[b-z][B-Z][1-8]"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Character Class Modify Range [a-z][A-Z][0-9]") {
    val pattern = "[a-z][A-Z][0-9]"
    val parsedTree = Parser(pattern).get

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(CharClassRangeModification))
    assertEquals(clue(mutants).length, 6)

    val expected: Seq[String] = Seq(
      // [a-z] -> [b-z] or [a-y]
      "[b-z][A-Z][0-9]",
      "[a-y][A-Z][0-9]",
      // [A-Z] -> [B-Z] OR [A-Y]
      "[a-z][B-Z][0-9]",
      "[a-z][A-Y][0-9]",
      // [0-9] -> [1-9] OR [0-8]
      "[a-z][A-Z][1-9]",
      "[a-z][A-Z][0-8]"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Character Class Modify Range [b-b][B-B][1-1]") {
    val pattern = "[b-b][B-B][1-1]"
    val parsedTree = Parser(pattern).get

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(CharClassRangeModification))
    assertEquals(clue(mutants).length, 6)

    val expected: Seq[String] = Seq(
      // [b-b] -> [a-b] or [b-c]
      "[a-b][B-B][1-1]",
      "[b-c][B-B][1-1]",
      // [B-B] -> [A-B] OR [B-C]
      "[b-b][A-B][1-1]",
      "[b-b][B-C][1-1]",
      // [1-1] -> [0-1] OR [1-2]
      "[b-b][B-B][0-1]",
      "[b-b][B-B][1-2]"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Character Class Modify Range [a-a][A-A][0-0]") {
    val pattern = "[a-a][A-A][0-0]"
    val parsedTree = Parser(pattern).get

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(CharClassRangeModification))
    assertEquals(clue(mutants).length, 3)

    val expected: Seq[String] = Seq(
      // [a-a] -> [a-b]
      "[a-b][A-A][0-0]",
      // [A-A] -> [A-B]
      "[a-a][A-B][0-0]",
      // [0-0] -> [0-1]
      "[a-a][A-A][0-1]"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Character Class Modify Range [z-z][Z-Z][9-9]") {
    val pattern = "[z-z][Z-Z][9-9]"
    val parsedTree = Parser(pattern).get

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(CharClassRangeModification))
    assertEquals(clue(mutants).length, 3)

    val expected: Seq[String] = Seq(
      // [z-z] -> [y-z]
      "[y-z][Z-Z][9-9]",
      // [Z-Z] -> [Y-Z]
      "[z-z][Y-Z][9-9]",
      // [9-9] -> [8-9]
      "[z-z][Z-Z][8-9]"
    ).sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Remove greedy quantifier") {
    val pattern = "a?b*c+d{1}e{1,}f{1,2}"
    val parsedTree = Parser(pattern).get

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
    val parsedTree = Parser(pattern).get

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
    val parsedTree = Parser(pattern).get

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
    val parsedTree = Parser(pattern).get

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
    val parsedTree = Parser(pattern).get

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
    val parsedTree = Parser(pattern).get

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(QuantifierNOrMoreChange))
    assertEquals(clue(mutants).length, 1)

    val expected: Seq[String] = Seq("a{1}").sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Modify quantifier {n,m}") {
    val pattern = "a{0,0}b{0,1}c{1,2}"
    val parsedTree = Parser(pattern).get

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
    val parsedTree = Parser(pattern).get

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
    val parsedTree = Parser(pattern).get

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
    val parsedTree = Parser(pattern).get

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

  test("Change capturing group to non-capturing group") {
    val pattern = "(hello)"
    val parsedTree = Parser(pattern).get

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(GroupToNCGroup))
    assertEquals(clue(mutants).length, 1)

    val expected: Seq[String] = Seq("(?:hello)").sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }
}
