package weaponregex

import weaponregex.model.regextree._
import weaponregex.model._

import scala.scalajs.js.annotation._

/** Main facade of Weapon regeX
  */
object WeaponRegeX {

  def mutate(pattern: String, mutationLevel: int): Seq[Mutant]

  def mutate(pattern: String, mutators: Seq[Mutator], mutationLevel: int): Seq[Mutant]

  def mutate(pattern: String, mutators: Seq[Mutator]): Seq[Mutant]

}
