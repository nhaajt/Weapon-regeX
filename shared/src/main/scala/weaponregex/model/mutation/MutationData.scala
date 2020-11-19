package weaponregex.model.mutation

import weaponregex.model.Location

case class MutationData(name: String, location: Location, level: Int, description: String = "") {}
