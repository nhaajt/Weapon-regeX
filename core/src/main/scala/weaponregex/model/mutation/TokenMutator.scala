package weaponregex.model.mutation

import weaponregex.model.regextree.RegexTree
import scala.scalajs.js.annotation._

trait TokenMutator {
  @JSExport
  val name: String
  @JSExport
  val levels: Seq[Int]
  @JSExport
  val description: String = name

  final def apply(token: RegexTree): Seq[String] = mutate(token)

  def mutate(token: RegexTree): Seq[String]
}
