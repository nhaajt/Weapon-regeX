package weaponregex.model

import scala.scalajs.js.annotation._

/** A specific spot in the source code based on line and column.
  * Stryker uses zero-based indexes. So the first character in a file is at line 0, column 0.
  *
  * @param line line number
  * @param column column number
  */
@JSExportTopLevel("Position")
@JSExportAll
case class Position(line: Int, column: Int)
