package weaponregex.model.regextree

import weaponregex.model.Location

trait RegexTree {
  val children: Seq[RegexTree]

  val prefix: String
  val postfix: String

  val location: Location

  /** Build the tree into a String
    */
  lazy val build: String = buildWhile(_ => true)

  /** Build the tree into a String with a child replaced by a string.
    * @param child Child to be replaced
    * @param childString Replacement String
    * @return A String representation of the tree
    */
  def buildWith(child: RegexTree, childString: String): String

  /** Build the tree into a String while a predicate holds for a given child.
    * @param pred Predicate on a child
    * @return A String representation of the tree
    */
  def buildWhile(pred: RegexTree => Boolean): String
}
