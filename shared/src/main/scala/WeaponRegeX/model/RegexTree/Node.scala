package WeaponRegeX.model.RegexTree
import WeaponRegeX.model.Location

abstract class Node(override val children: RegexTree*)(override val location: Location)(implicit
    val prefix: String = "",
    val postfix: String = "",
    val sep: String = ""
) extends RegexTree {
  override def build: String = prefix + children.map(_.build).mkString(sep = sep) + postfix
}
