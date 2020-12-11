package weaponregex.model

import scala.scalajs.js.annotation._

/** A location in the source code which can span multiple lines and/or columns.
  *
  * @param start start position
  * @param end end position
  */
@JSExportTopLevel("Location")
@JSExportAll
case class Location(start: Position, end: Position)
