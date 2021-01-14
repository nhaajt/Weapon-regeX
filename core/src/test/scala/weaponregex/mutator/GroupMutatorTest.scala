package weaponregex.mutator

import weaponregex.parser.Parser
import TreeMutator._
import weaponregex.model.mutation.Mutant

class GroupMutatorTest extends munit.FunSuite {
  test("Changes capturing group to non-capturing group") {
    val pattern = "(hello)"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(GroupToNCGroup))
    assertEquals(clue(mutants).length, 1)

    val expected: Seq[String] = Seq("(?:hello)").sorted
    assertEquals(clue(mutants).map(_.pattern).sorted, expected)
  }

  test("Does not change escaped capturing groups") {
    val pattern = "\\(hello\\)"
    val parsedTree = Parser.parseOrError(pattern)

    val mutants: Seq[Mutant] = parsedTree.mutate(Seq(GroupToNCGroup))
    assertEquals(clue(mutants).length, 0)
  }
}
