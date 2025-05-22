package dev.lisek.needleman_wunsch.algorithm

import scala.math.max
import java.util.Arrays
import java.io.FileWriter
import dev.lisek.needleman_wunsch.plot.plot
import dev.lisek.needleman_wunsch.util.StringUtils.output
import dev.lisek.needleman_wunsch.util.StringUtils.resultString
import dev.lisek.needleman_wunsch.util.Parameters

/**
  * Tracks the optimal alignments.
  *
  * @param par Parsed command-line parameters
  * @param file FileWriter to write to
  */
class Track(
    par: Parameters,
    file: FileWriter,
    seq1: (String, String),
    seq2: (String, String)
):
    val matrix = heatmap(par, seq1._2, seq2._2)
    val score = matrix(seq1._2.length)(seq2._2.length)
    var tracks: Array[(String, String)] = Array()

    /** Backtracks the matrix to find the optimal alignments. */
    def backtrack: Unit =
        stepBack(
            seq1._2.length,
            seq2._2.length
        )

        if !par.quiet then
            output(file, s"Similarity score: ${matrix(seq1._2.length)(seq2._2.length)}\n\n")

        // Close the FileWriter
        // if file != null then
        //     file.close

        if (par.createGraph)
            plot(seq1, seq2, matrix, tracks)

    /**
      * Recursively performs a backtrack step.
      *
      * @param y Position in seq1
      * @param x Position in seq2
      * @param alignSeq1 Sequence 1 aligned with gaps
      * @param alignSeq2 Sequence 2 aligned with gaps
      */
    private def stepBack(
        y: Int,
        x: Int,
        alignSeq1: Seq[Char] = Seq(),
        alignSeq2: Seq[Char] = Seq()
    ): Unit =
        if tracks.length >= par.maxTracks then return

        var ret: Array[(String, String)] = Array.empty[(String, String)]

        // Finish recursion on boundary
        if (y == 0 || x == 0)
            // Take remaining characters
            var sub1 = seq1._2.take(y)
            var sub2 = seq2._2.take(x)

            // Pad the other sequence
            if (y > 0)
                sub2 = "-" * (sub1.size) ++ sub2
            else if (x > 0)
                sub1 = "-" * (sub2.size) ++ sub1

            // Create final tracks
            val track1 = (sub1 +: alignSeq1).mkString
            val track2 = (sub2 +: alignSeq2).mkString

            if !par.quiet then
                output(file, resultString(track1, track2))

            // Add to tracks
            tracks = tracks :+ (track1.mkString, track2.mkString)

        else    // Check possible moves
            if (matrix(y)(x) == matrix(y-1)(x-1) + (if (seq1._2(y-1) == seq2._2(x-1)) par.matchValue else par.mismatchValue))
                // Diagonal move (match/mismatch)
                stepBack(y - 1, x - 1, seq1._2(y-1) +: alignSeq1, seq2._2(x-1) +: alignSeq2)
            if (matrix(y)(x) == matrix(y-1)(x) + par.gapValue)
                // Up move (gap in seq2)
                stepBack(y - 1, x, seq1._2(y-1) +: alignSeq1, '-' +: alignSeq2)
            if (matrix(y)(x) == matrix(y)(x-1) + par.gapValue)
                // Left move (gap in seq1)
                stepBack(y, x - 1, '-' +: alignSeq1, seq2._2(x-1) +: alignSeq2)
