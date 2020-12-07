package weaponregex

import weaponregex.model.regextree._
import weaponregex.model._

import scala.scalajs.js.annotation._

@JSExportTopLevel("WRegex")
class WRegex {
  val pattern: String
  val flags: String = ""
}

/** Main facade of Weapon regeX
  */
@JSExportTopLevel("WeaponRegeX")
object WeaponRegeX {

  @JSExport
  def mutate(pattern: WRegex, mutationLevel: int)

  @JSExport
  def mutate(pattern: WRegex, mutators: Seq[Mutator], mutationLevel: int)

  @JSExport
  def mutate(pattern: WRegex, mutators: Seq[Mutator])
}
