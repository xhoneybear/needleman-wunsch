package dev.lisek.needleman_wunsch.plot

import scala.collection.mutable.Buffer

/**
  * Converts tracks from readable text to plottable points.
  *
  * @param seq1 First sequence
  * @param seq2 Second sequence
  * @return Sequence of points
  */
def tracksToPoints(seq1: String, seq2: String): Seq[(Double, Double)] =
    var x = 0
    var y = 0

    val points = Buffer[(Double, Double)]()  // Use a mutable buffer to store points

    points += ((x, y))  // Add initial point

    for (i <- 0 until seq1.length)
        if (seq1(i) != '-') y += 1  // Increment y if not a gap in seq1
        if (seq2(i) != '-') x += 1  // Increment x if not a gap in seq2

        points += ((x, y))  // Add the current point

    points.toSeq  // Convert to immutable sequence to return
