package WeaponRegeX

import model.RegexTree._
import model._

/** Main facade of Weapon regeX
  */
object WeaponRegeX {
  def parse(pattern: String): RegexTree = {
    val loc = Location(Position(0, 0), Position(0, 1))
    Character('?', loc)
  }
}
