package WeaponRegeX.model.RegexTree
import WeaponRegeX.model.Location

case class CharacterClass(nodes: Seq[RegexTree], isNegative: Boolean, loc: Location)
    extends Node(nodes: _*)(loc)(prefix = if (isNegative) "[^" else "[", postfix = "]") {}
