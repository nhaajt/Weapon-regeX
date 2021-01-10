package weaponregex.model.mutation

import weaponregex.model.regextree.RegexTree
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSConverters._
import scala.scalajs.js

trait TokenMutator {

  /** The name of the mutator
    */
  @JSExport
  val name: String

  /** The mutation levels that the token mutator falls under
    */
  val levels: Seq[Int]
  @JSExport("levels")
  def levelsJS: js.Array[Int] = levels.toJSArray

  /** A short description  of the mutator
    */
  @JSExport
  val description: String = name

  /** Apply mutation to the given token
    * @param token Target token
    * @return Sequence of strings, which are mutations of the original token
    */
  final def apply(token: RegexTree): Seq[String] = mutate(token)

  /** Mutate the given token
    * @param token Target token
    * @return Sequence of strings, which are mutations of the original token
    */
  def mutate(token: RegexTree): Seq[String]
}
