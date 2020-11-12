package WeaponRegeX

import model.RegexTree._
import model._

/** Main facade of Weapon regeX
  */
object WeaponRegeX {
  def main(args: Array[String]): Unit = {
    val pattern: String = """^\w+@[a-zA-Z_]+?\.[a-zA-Z]{2,3}$"""
    val loc = Location(Position(0, 0), Position(0, 1))
    val tree: RegexTree = Concat(
      Seq(
        Boundary("^", loc),
        OneOrMore(
          PredefinedCharClass("w", isPositive = true, loc),
          loc
        ),
        Character('@', loc),
        OneOrMore(
          CharacterClass(
            Seq(
              Range(Character('a', loc), Character('z', loc), loc),
              Range(Character('A', loc), Character('Z', loc), loc),
              Character('_', loc)
            ),
            loc
          ),
          loc,
          isReluctant = true
        ),
        QuoteChar('.', loc),
        Quantifier(
          CharacterClass(
            Seq(
              Range(Character('a', loc), Character('z', loc), loc),
              Range(Character('A', loc), Character('Z', loc), loc)
            ),
            loc
          ),
          min = 2,
          hasComma = true,
          max = 3,
          loc
        ),
        Boundary("$", loc)
      ),
      loc
    )
    val result: String = tree.build
    println(pattern)
    println(result)
    println(if (result == pattern) "MATCH" else "NOT MATCH")
  }
}
