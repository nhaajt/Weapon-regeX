package weaponregex.model.regextree

import weaponregex.model.Location

trait RegexTree {
  // Won't only a Node have children, and a Leaf never? This feels like it would make more sense in the Node trait
  val children: Seq[RegexTree]

  val prefix: String = ""
  val postfix: String = ""

  val location: Location

  // Why these 3 functions that all do the same? `pattern` is lazy, but I couldn't find in your report that this was a performance issue
  // If it's not I would keep it a def for simplicity sake. Most times it will only be called once anyway, right?
  // Having the 3 functions that do the same is also a little confusing from an API standpoint

  final lazy val pattern: String = _build

  final def build: String = pattern

  protected def _build: String

  // This could use some scaladoc about what exactly it does 😃
  def buildWith(child: RegexTree, childString: String): String
}
