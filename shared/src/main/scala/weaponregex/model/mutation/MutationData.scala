package weaponregex.model.mutation

import weaponregex.model.Location

/** Data structure for metadata of a mutation made by a mutator.
  * @param name Name of the mutation
  * @param location [[Location]] in the original string where the mutation occurred
  * @param level The highest mutation level of the mutator
  * @param description Description on the mutation
  */
case class MutationData(name: String, location: Location, level: Int, description: String = "") {}
