package WeaponRegeX.model.RegexTree

import WeaponRegeX.model.Location

abstract class Leaf(val value: String)(override val location: Location) extends RegexTree {
  override val children: Seq[RegexTree] = Nil
}
