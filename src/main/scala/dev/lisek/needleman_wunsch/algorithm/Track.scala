package dev.lisek.needleman_wunsch.algorithm

import scala.math.max
import java.util.Arrays
import java.io.FileWriter
import dev.lisek.needleman_wunsch.plot.plot
import dev.lisek.needleman_wunsch.util.StringUtils.output
import dev.lisek.needleman_wunsch.util.StringUtils.resultString

/** Tracks the optimal alignments. */
object Track:
    var tracks: Array[(String, String)] = Array()

    /**
      * Backtracks the matrix to find the optimal alignments.
      *
      * @param file FileWriter to write to
      * @param matrix Matrix to backtrack
      * @param seq1 First sequence with title
      * @param seq2 Second sequence with title
      * @param matchValue Points to add if characters match
      * @param gapValue Points to add if there is a gap
      * @param mismatchValue Points to add if characters don't match
      * @param maxTracks Maximum number of tracks to calculate
      * @param createGraph Whether to create a graph
      */
    def backtrack(
        file: FileWriter,
        matrix: Array[Array[Double]],
        seq1: (String, String),
        seq2: (String, String),
        matchValue: Int,
        gapValue: Int,
        mismatchValue: Int,
        maxTracks: Int,
        createGraph: Boolean
    ): Unit =
        var out = "--- Evaluation results ---\n"
        out ++= s"\nSequence 1: ${seq1(0)}"
        out ++= s"\nSequence 2: ${seq2(0)}\n"
        out ++= "\nConnections:\n\n"

        output(file, out)

        stepBack(
            file,
            matrix,
            seq1(1),
            seq2(1),
            matchValue,
            gapValue,
            mismatchValue,
            maxTracks,
            seq1(1).length,
            seq2(1).length
        )

        out = s"Similarity score: ${matrix(seq1(1).length)(seq2(1).length)}"

        output(file, out)

        // Close the FileWriter
        if file != null then
            file.close

        if (createGraph)
            plot(seq1, seq2, matrix, tracks)

    /**
      * Recursively performs a backtrack step.
      *
      * @param file FileWriter to write to
      * @param matrix Matrix to backtrack
      * @param seq1 First sequence
      * @param seq2 Second sequence
      * @param matchValue Points to add if characters match
      * @param gapValue Points to add if there is a gap
      * @param mismatchValue Points to add if characters don't match
      * @param maxTracks Maximum number of tracks to calculate
      * @param y Dynamic position in seq1
      * @param x Dynamic position in seq2
      * @param alignSeq1 Calculated sequence 1 with gaps
      * @param alignSeq2 Calculated sequence 2 with gaps
      */
    private def stepBack(
        file: FileWriter,
        matrix: Array[Array[Double]],
        seq1: String,
        seq2: String,
        matchValue: Int,
        gapValue: Int,
        mismatchValue: Int,
        maxTracks: Int,
        y: Int = -1,
        x: Int = -1,
        alignSeq1: Seq[Char] = Seq(),
        alignSeq2: Seq[Char] = Seq()
    ): Unit =
        if tracks.length >= maxTracks then return

        var ret: Array[(String, String)] = Array.empty[(String, String)]

        // Finish recursion on boundary
        if (y == 0 || x == 0)
            // Take remaining characters
            var sub1 = seq1.take(y)
            var sub2 = seq2.take(x)

            // Pad the other sequence
            if (y > 0)
                sub2 = "-" * (sub1.size) ++ sub2
            else if (x > 0)
                sub1 = "-" * (sub2.size) ++ sub1

            // Create final tracks
            val track1 = (sub1 +: alignSeq1).mkString
            val track2 = (sub2 +: alignSeq2).mkString

            output(file, resultString(track1, track2))

            // Add to tracks
            tracks = tracks :+ (track1.mkString, track2.mkString)

        else    // Check possible moves
            if (matrix(y)(x) == matrix(y-1)(x-1) + (if (seq1(y-1) == seq2(x-1)) matchValue else mismatchValue))
                // Diagonal move (match/mismatch)
                stepBack(file, matrix, seq1, seq2, matchValue, gapValue, mismatchValue, maxTracks, y - 1, x - 1, seq1(y-1) +: alignSeq1, seq2(x-1) +: alignSeq2)
            if (matrix(y)(x) == matrix(y-1)(x) + gapValue)
                // Up move (gap in seq2)
                stepBack(file, matrix, seq1, seq2, matchValue, gapValue, mismatchValue, maxTracks, y - 1, x, seq1(y-1) +: alignSeq1, '-' +: alignSeq2)
            if (matrix(y)(x) == matrix(y)(x-1) + gapValue)
                // Left move (gap in seq1)
                stepBack(file, matrix, seq1, seq2, matchValue, gapValue, mismatchValue, maxTracks, y, x - 1, '-' +: alignSeq1, seq2(x-1) +: alignSeq2)
