package weaponregex.model.mutation

import weaponregex.model.Location
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExport

/** A mutation made by the mutator.
  * @param pattern The replacement pattern
  * @param name Name of the mutation
  * @param location [[weaponregex.model.Location]] in the original string where the mutation occurred
  * @param levels The mutation levels of the mutator
  * @param description Description on the mutation
  */
case class Mutant(
    @JSExport pattern: String,
    @JSExport name: String,
    @JSExport location: Location,
    levels: Seq[Int],
    @JSExport description: String
) {
  @JSExport val mutationLevels: js.Array[Int] = levels.toJSArray
}
