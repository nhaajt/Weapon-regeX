package weaponregex

import weaponregex.mutator.BuiltinMutators
import weaponregex.model.mutation.Mutant

class WeaponRegeXTest extends munit.FunSuite {
  test("Can mutate without options") {
    var mutations = WeaponRegeX.mutate("^a")
    assertEquals(mutations.length, 2)
    assert(mutations.isInstanceOf[Seq[Mutant]])
  }

  test("Can mutate with only mutators as option") {
    var mutations = WeaponRegeX.mutate("^a", BuiltinMutators.all)
    assertEquals(mutations.length, 2)
    assert(mutations.isInstanceOf[Seq[Mutant]])
  }

  test("Can mutate with only levels as option") {
    var mutations = WeaponRegeX.mutate("^a", mutationLevels = Seq(1))
    assertEquals(mutations.length, 1)
    assert(mutations.isInstanceOf[Seq[Mutant]])
  }

  test("Returns an empty sequence if there are no mutants") {
    var mutations = WeaponRegeX.mutate("a")
    assertEquals(mutations, Seq())
  }

  test("Returns an empty array if the RegEx is invalid") {
    var mutations = WeaponRegeX.mutate("*(a|$]")
    assertEquals(mutations, Seq())
  }
}
