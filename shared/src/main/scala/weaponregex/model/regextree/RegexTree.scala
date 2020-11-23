package weaponregex.model.regextree

import weaponregex.model.Location

trait RegexTree {
  val children: Seq[RegexTree]

  val prefix: String = ""
  val postfix: String = ""

  val location: Location

  final lazy val pattern: String = _build

  final def build: String = pattern

  protected def _build: String

  def buildWith(child: RegexTree, childString: String): String
}
