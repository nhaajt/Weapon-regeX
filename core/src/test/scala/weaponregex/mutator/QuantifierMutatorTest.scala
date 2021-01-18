package weaponregex.mutator

import weaponregex.parser.Parser
import TreeMutator._

class QuantifierMutatorTest extends munit.FunSuite {
  test("Removes greedy quantifier") {
    val pattern = "a?b*c+d{1}e{1,}f{1,2}g"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[String] = parsedTree.mutate(Seq(QuantifierRemoval)) map (_.pattern)
    assertEquals(clue(mutants).length, 6)

    val expected: Seq[String] = Seq(
      "ab*c+d{1}e{1,}f{1,2}g",
      "a?bc+d{1}e{1,}f{1,2}g",
      "a?b*cd{1}e{1,}f{1,2}g",
      "a?b*c+de{1,}f{1,2}g",
      "a?b*c+d{1}ef{1,2}g",
      "a?b*c+d{1}e{1,}fg"
    )

    mutants foreach (m => assert(clue(expected) contains clue(m)))
  }

  test("Does not remove escaped greedy quantifiers") {
    val pattern = """a\?b\*c\+d\{1\}e\{1,\}f\{1,2\}g"""
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[String] = parsedTree.mutate(Seq(QuantifierRemoval)) map (_.pattern)
    assertEquals(clue(mutants), Nil)
  }

  test("Removes reluctant quantifier") {
    val pattern = "a??b*?c+?d{1}?e{1,}?f{1,2}?g"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[String] = parsedTree.mutate(Seq(QuantifierRemoval)) map (_.pattern)
    assertEquals(clue(mutants).length, 6)

    val expected: Seq[String] = Seq(
      "ab*?c+?d{1}?e{1,}?f{1,2}?g",
      "a??bc+?d{1}?e{1,}?f{1,2}?g",
      "a??b*?cd{1}?e{1,}?f{1,2}?g",
      "a??b*?c+?de{1,}?f{1,2}?g",
      "a??b*?c+?d{1}?ef{1,2}?g",
      "a??b*?c+?d{1}?e{1,}?fg"
    )

    mutants foreach (m => assert(clue(expected) contains clue(m)))
  }

  test("Does not remove escaped greedy quantifiers") {
    val pattern = """a\?\?b\*\?c\+\?d\{1\}\?e\{1,\}\?f\{1,2\}\?g"""
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[String] = parsedTree.mutate(Seq(QuantifierRemoval)) map (_.pattern)
    assertEquals(clue(mutants), Nil)
  }

  test("Removes possessive quantifier") {
    val pattern = "a?+b*+c++d{1}+e{1,}+f{1,2}+"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[String] = parsedTree.mutate(Seq(QuantifierRemoval)) map (_.pattern)
    assertEquals(clue(mutants).length, 6)

    val expected: Seq[String] = Seq(
      "ab*+c++d{1}+e{1,}+f{1,2}+",
      "a?+bc++d{1}+e{1,}+f{1,2}+",
      "a?+b*+cd{1}+e{1,}+f{1,2}+",
      "a?+b*+c++de{1,}+f{1,2}+",
      "a?+b*+c++d{1}+ef{1,2}+",
      "a?+b*+c++d{1}+e{1,}+f"
    )

    mutants foreach (m => assert(clue(expected) contains clue(m)))
  }

  test("Does not remove escaped possessive quantifiers") {
    val pattern = """a\?\+b\*\+c\+\+d\{1\}\+e\{1,\}\+f\{1,2\}\+g"""
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[String] = parsedTree.mutate(Seq(QuantifierRemoval)) map (_.pattern)
    assertEquals(clue(mutants), Nil)
  }

  test("Changes quantifier {n}") {
    val pattern = "a{1}"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[String] = parsedTree.mutate(Seq(QuantifierNChange)) map (_.pattern)
    assertEquals(clue(mutants).length, 2)

    val expected: Seq[String] = Seq(
      "a{0,1}",
      "a{1,}"
    )

    mutants foreach (m => assert(clue(expected) contains clue(m)))
  }

  test("Modifies quantifier {n,}") {
    val pattern = "a{0,}b{1,}"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[String] = parsedTree.mutate(Seq(QuantifierNOrMoreModification)) map (_.pattern)
    assertEquals(clue(mutants).length, 3)

    val expected: Seq[String] = Seq(
      "a{1,}b{1,}",
      "a{0,}b{0,}",
      "a{0,}b{2,}"
    )

    mutants foreach (m => assert(clue(expected) contains clue(m)))
  }

  test("Changes quantifier {n,}") {
    val pattern = "a{1,}"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[String] = parsedTree.mutate(Seq(QuantifierNOrMoreChange)) map (_.pattern)
    assertEquals(clue(mutants).length, 1)

    val expected: Seq[String] = Seq("a{1}")

    mutants foreach (m => assert(clue(expected) contains clue(m)))
  }

  test("Modifies quantifier {n,m}") {
    val pattern = "a{0,0}b{0,1}c{1,2}"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[String] = parsedTree.mutate(Seq(QuantifierNMModification)) map (_.pattern)
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
    )

    mutants foreach (m => assert(clue(expected) contains clue(m)))
  }

  test("Modifies short quantifier") {
    val pattern = "a?b*c+"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[String] = parsedTree.mutate(Seq(QuantifierShortModification)) map (_.pattern)
    assertEquals(clue(mutants).length, 6)

    val expected: Seq[String] = Seq(
      "a{1,1}b*c+",
      "a{0,0}b*c+",
      "a{0,2}b*c+",
      "a?b{1,}c+",
      "a?b*c{0,}",
      "a?b*c{2,}"
    )

    mutants foreach (m => assert(clue(expected) contains clue(m)))
  }

  test("Changes short quantifier") {
    val pattern = "a*b+"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[String] = parsedTree.mutate(Seq(QuantifierShortChange)) map (_.pattern)
    assertEquals(clue(mutants).length, 2)

    val expected: Seq[String] = Seq(
      "a{0}b+",
      "a*b{1}"
    )

    mutants foreach (m => assert(clue(expected) contains clue(m)))
  }

  test("Adds reluctant to greedy quantifier") {
    val pattern = "a?b*c+d{1}e{1,}f{1,2}"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[String] = parsedTree.mutate(Seq(QuantifierReluctantAddition)) map (_.pattern)
    assertEquals(clue(mutants).length, 6)

    val expected: Seq[String] = Seq(
      "a??b*c+d{1}e{1,}f{1,2}",
      "a?b*?c+d{1}e{1,}f{1,2}",
      "a?b*c+?d{1}e{1,}f{1,2}",
      "a?b*c+d{1}?e{1,}f{1,2}",
      "a?b*c+d{1}e{1,}?f{1,2}",
      "a?b*c+d{1}e{1,}f{1,2}?"
    )

    mutants foreach (m => assert(clue(expected) contains clue(m)))
  }
}
