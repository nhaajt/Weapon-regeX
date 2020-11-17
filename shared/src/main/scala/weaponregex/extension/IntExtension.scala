package weaponregex.`extension`

import weaponregex.model._

import scala.annotation.tailrec

object IntExtension {
  implicit class IndexExtension(index: Int) {

    /** Convert an index of into row and column numbers in a given sequence. If the index exceeds the sequence length then it will be considered as the index of the last element.
      * @param seq A sequence of elements
      * @return A tuple of row and column numbers
      */
    @tailrec
    final def toRowCol[A](seq: Seq[A]): (Int, Int) = if (index < seq.length)
      seq.take(index + 1).foldLeft((0, 0)) { case ((row, column), c) =>
        if (c == '\n') (row + 1, 0)
        else (row, column + 1)
      }
    else (seq.length - 1) toRowCol seq

    /** Convert an index into a [[Position]] in a given sequence. If the index exceeds the sequence length then it will be considered as the index of the last element.
      * @param seq A sequence of elements
      * @return A [[Position]]
      */
    final def positionIn[A](seq: Seq[A]): Position = {
      val (line, column) = index toRowCol seq
      Position(line, column)
    }
  }

  implicit class IndexPairExtension(ij: (Int, Int)) {

    /** Convert an index pair into a [[Location]] in a given sequence. If any index exceeds the sequence length then it will be considered as the index of the last element.
      * @param seq A sequence of elements
      * @return A [[Location]]
      */
    final def locationIn[A](seq: Seq[A]): Location = {
      val (i, j) = ij
      Location(i positionIn seq, j positionIn seq)
    }
  }
}
