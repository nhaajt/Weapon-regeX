package weaponregex.extension

import fastparse.internal.Util
import weaponregex.model._

object StringExtension {
  implicit class StringIndexExtension(string: String) {

    /** Convert an index of into row and column numbers in the given string.
      *
      * @param index An index
      * @return A tuple of row and column numbers
      *
      * @note This function implementation is taken from [[https://github.com/lihaoyi/fastparse/blob/master/fastparse/src/fastparse/ParserInput.scala here]]
      */
    final def toLineCol(index: Int): (Int, Int) = {
      val lineNumberLookup = Util.lineNumberLookup(string)
      val line = lineNumberLookup.indexWhere(_ > index) match {
        case -1 => lineNumberLookup.length - 1
        case n  => math.max(0, n - 1)
      }
      val col = index - lineNumberLookup(line)
      (line, col)
    }

    /** Convert an index into a [[weaponregex.model.Position]] in the given string.
      * @param index An index
      * @return A [[weaponregex.model.Position]]
      */
    final def positionOf(index: Int): Position = {
      val (line, column) = string toLineCol index
      Position(line, column)
    }

    /** Convert a pair of start and end indices into a [[weaponregex.model.Location]] in the given string.
      * @param start Start index
      * @param end End index
      * @return A [[weaponregex.model.Location]]
      */
    final def locationOf(start: Int, end: Int): Location = Location(string positionOf start, string positionOf end)
  }

  // Are these used anywhere?
  implicit class StringStylingExtension(string: String) {
    final def style(style: String): String = style + string + Console.RESET

    final def black: String = style(Console.BLACK)

    final def red: String = style(Console.RED)

    final def green: String = style(Console.GREEN)

    final def yellow: String = style(Console.YELLOW)

    final def blue: String = style(Console.BLUE)

    final def magenta: String = style(Console.MAGENTA)

    final def cyan: String = style(Console.CYAN)

    final def white: String = style(Console.WHITE)

    final def bold: String = style(Console.BOLD)

    final def underlined: String = style(Console.UNDERLINED)

    final def toggleCase: String = string map (char => if (char.isUpper) char.toLower else char.toUpper)
  }
}
