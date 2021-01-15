package weaponregex.mutator

import weaponregex.parser.Parser
import weaponregex.mutator.TreeMutator._
import weaponregex.model.regextree.RegexTree

class TreeMutatorTest extends munit.FunSuite {

  var tree: RegexTree = null

  override def beforeEach(context: BeforeEach): Unit = {
    tree = Parser("^(a*|b+|[[c-z]XYZ]{3,}(ABC{4}DEF{5,9}\\w))$").get
  }

  test("Filters mutators with level 1") {
    val mutants = tree.mutate(BuiltinMutators.all, Seq(1))
    mutants foreach (m => assert(m.levels.contains(1)))
    assert(mutants exists (m => m.levels.length != 1))
  }

  test("Filters mutators with level 2") {
    val mutants = tree.mutate(BuiltinMutators.all, Seq(2))
    mutants foreach (m => assert(m.levels.contains(2)))
    assert(mutants exists (m => m.levels.length != 1))
  }

  test("Filters mutators with level 3") {
    val mutants = tree.mutate(BuiltinMutators.all, Seq(3))
    mutants foreach (m => assert(m.levels.contains(3)))
    assert(mutants exists (m => m.levels.length != 1))
  }

  test("Filters mutators with levels 1, 2") {
    val mutants = tree.mutate(BuiltinMutators.all, Seq(1, 2))
    mutants foreach (m => assert(Seq(1, 2).exists(m.levels.contains(_))))
    assert(mutants exists (m => m.levels.length == 1))
  }

  test("Filters mutators with levels 2, 3") {
    val mutants = tree.mutate(BuiltinMutators.all, Seq(2, 3))
    mutants foreach (m => assert(Seq(2, 3).exists(m.levels.contains(_))))
    assert(mutants exists (m => m.levels.length == 1))
  }

  test("Filters mutators with levels 1, 3") {
    val mutants = tree.mutate(BuiltinMutators.all, Seq(1, 3))
    mutants foreach (m => assert(Seq(1, 3).exists(m.levels.contains(_))))
    assert(mutants exists (m => m.levels.length == 1))
  }

  test("Filters mutators with levels 1, 2, 3") {
    val mutants = tree.mutate(BuiltinMutators.all, Seq(1, 2, 3))
    mutants foreach (m => assert(Seq(1, 2, 3).exists(m.levels.contains(_))))
    assert(mutants exists (m => m.levels.length == 1))
  }

  test("Mutates with all mutators by default") {
    val mutants = tree.mutate()
    BuiltinMutators.all foreach (mutator => assert(mutants.exists(mutant => mutant.name equals mutator.name)))
  }

  test("No filtering is done by default") {
    val mutants = tree.mutate(BuiltinMutators.all)
    BuiltinMutators.all foreach (mutator => assert(mutants.exists(mutant => mutant.name equals mutator.name)))
  }

  test("Uses only the given mutators") {
    val usedMutators = BuiltinMutators.all.take(5)
    val excludedMutators = BuiltinMutators.all.drop(5)
    val mutants = tree.mutate(usedMutators)
    usedMutators foreach (mutator => assert(mutants.exists(mutant => mutant.name equals mutator.name)))
    excludedMutators foreach (mutator => assert(mutants.forall(mutant => !(mutant.name equals mutator.name))))
  }
}
