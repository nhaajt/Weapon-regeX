package weaponregex

import weaponregex.model.regextree._
import weaponregex.model._

import scala.scalajs.js.annotation._

/** Main facade of Weapon regeX
  */
@JSExportTopLevel("WeaponRegeX")
object WeaponRegeX {

  @JSExport
  def parse(pattern: String): RegexTree = {
    val loc = Location(Position(0, 0), Position(0, 1))
    Character('?', loc)
  }
}
