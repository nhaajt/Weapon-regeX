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

  /** Build the tree into a String while a predicate holds for a given child.
    * @param pred Predicate on a child
    * @return A String representation of the tree
    */
  def buildWhile(pred: RegexTree => Boolean): String
}
