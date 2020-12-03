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
}
