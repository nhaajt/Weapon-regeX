package weaponregex.model

/** A location in the source code which can span multiple lines and/or columns.
  *
  * @param start start position
  * @param end end position
  */
case class Location(start: Position, end: Position)
