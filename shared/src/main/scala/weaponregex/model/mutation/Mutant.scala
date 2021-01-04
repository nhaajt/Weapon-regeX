package weaponregex.model.mutation

import scala.scalajs.js.annotation._
import weaponregex.model.Location

/** A mutation made by the mutator.
  * @param pattern The replacement pattern
  * @param name Name of the mutation
  * @param location [[weaponregex.model.Location]] in the original string where the mutation occurred
  * @param levels The mutation levels of the mutator
  * @param description Description on the mutation
  */
@JSExportAll
case class Mutant(pattern: String, name: String, location: Location, levels: Seq[Int], description: String)
