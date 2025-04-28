package dev.lisek.needleman_wunsch.algorithm

import scala.collection.mutable.Buffer
import scala.math.max
import java.util.Arrays
import java.io.FileWriter
import dev.lisek.needleman_wunsch.plot.Plot.plot
import dev.lisek.needleman_wunsch.util.StringUtils.output
import dev.lisek.needleman_wunsch.util.StringUtils.resultString

object Track:
    var tracks: Array[(String, String)] = Array()

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
        out ++= "\nConnections:\n"

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

        out = s"\nSimilarity score: ${matrix(seq1(1).length)(seq2(1).length)}"

        output(file, out)

        if file != null then
            file.close

        if (createGraph)
            plot(seq1, seq2, matrix, tracks)

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

        if (y == 0 || x == 0)
            var sub1 = seq1.take(y)
            var sub2 = seq2.take(x)
            if (y > 0)
                sub2 = "-" * (sub1.size) ++ sub2
            else if (x > 0)
                sub1 = "-" * (sub2.size) ++ sub1
            val trace1 = (sub1 +: alignSeq1).mkString
            val trace2 = (sub2 +: alignSeq2).mkString

            var out = resultString(trace1, trace2)

            output(file, out)

            tracks = tracks :+ (trace1.mkString, trace2.mkString)

        else
            if (matrix(y)(x) == matrix(y-1)(x-1) + (if (seq1(y-1) == seq2(x-1)) matchValue else mismatchValue))
                // Diagonal move (match/mismatch)
                stepBack(file, matrix, seq1, seq2, matchValue, gapValue, mismatchValue, maxTracks, y - 1, x - 1, seq1(y-1) +: alignSeq1, seq2(x-1) +: alignSeq2)
            if (matrix(y)(x) == matrix(y-1)(x) + gapValue)
                // Up move (gap in seq2)
                stepBack(file, matrix, seq1, seq2, matchValue, gapValue, mismatchValue, maxTracks, y - 1, x, seq1(y-1) +: alignSeq1, '-' +: alignSeq2)
            if (matrix(y)(x) == matrix(y)(x-1) + gapValue)
                // Left move (gap in seq1)
                stepBack(file, matrix, seq1, seq2, matchValue, gapValue, mismatchValue, maxTracks, y, x - 1, '-' +: alignSeq1, seq2(x-1) +: alignSeq2)

    def tracePath(seq1: String, seq2: String): Seq[(Double, Double)] =
        var x = 0
        var y = 0

        val points = Buffer[(Double, Double)]()  // Use a mutable buffer to store points

        points += ((x, y))  // Add initial point

        for (i <- 0 until seq1.length)
            if (seq1(i) != '-') y += 1  // Increment y if not a gap in seq1
            if (seq2(i) != '-') x += 1  // Increment x if not a gap in seq2

            points += ((x, y))  // Add the current point

        points.toSeq  // Convert to immutable sequence to return
