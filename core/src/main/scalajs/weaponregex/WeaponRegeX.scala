package weaponregex

import weaponregex.parser.Parser
import weaponregex.mutator.TreeMutator._
import weaponregex.model.mutation._
import weaponregex.mutator.BuiltinMutators

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation._

// class WRegex(var pattern: String, var flags: String = "")

/** Main facade of Weapon regeX
  */
object WeaponRegeX {

  @JSExportTopLevel("mutators")
  val allMutators = BuiltinMutators.all.map(mut => mut.getClass().getSimpleName().dropRight(1) -> mut).toMap.toJSMap

  class OptionObject extends js.Object {
    val mutators: js.Array[TokenMutator] = BuiltinMutators.all.toJSArray
    val mutationLevels: js.Array[Int] = js.Array(1, 2, 3)
  }

  @JSExportTopLevel("mutate")
  def mutate(pattern: String, options: OptionObject = new OptionObject {}): js.Array[Mutant] =
    Parser(pattern) match {
      case Some(tree) =>
        tree
          .mutate(
            if (options.hasOwnProperty("mutators")) options.mutators.toSeq else BuiltinMutators.all,
            if (options.hasOwnProperty("mutationLevels")) options.mutationLevels.toSeq else null
          )
          .toJSArray
      case None => js.Array()
    }
}
