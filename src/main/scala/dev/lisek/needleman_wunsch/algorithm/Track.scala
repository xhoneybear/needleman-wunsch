package dev.lisek.needleman_wunsch.algorithm

import scala.collection.mutable.Buffer
import scala.math.max
import java.util.Arrays
import java.io.FileWriter

object Track:
    def heatmap(
        seq1: String,
        seq2: String,
        matchValue: Int,
        gapValue: Int,
        mismatchValue: Int
    ): Array[Array[Double]] =
        var grid = Array.ofDim[Double](seq1.length + 1, seq2.length + 1)

        for i <- 0 to seq1.length do
            grid(i)(0) = gapValue * i

        for j <- 0 to seq2.length do
            grid(0)(j) = gapValue * j

        for i <- 1 to seq1.length do
            for j <- 1 to seq2.length do
                val matchScore = if seq1(i - 1) == seq2(j - 1) then matchValue else mismatchValue
                val score = grid(i - 1)(j - 1) + matchScore
                val delete = grid(i - 1)(j) + gapValue
                val insert = grid(i)(j - 1) + gapValue
                grid(i)(j) =
                    if (score > delete && score > insert) score
                    else if (delete > insert) delete
                    else insert
        grid

    def backtrack(
        file: FileWriter,
        matrix: Array[Array[Double]],
        seq1: String,
        seq2: String,
        matchValue: Int,
        gapValue: Int,
        mismatchValue: Int,
        y: Int = -1,
        x: Int = -1,
        alignSeq1: Seq[Char] = Seq(),
        alignSeq2: Seq[Char] = Seq()
    ): Unit =
        if (y == -1 || x == -1)
            return backtrack(
                file,
                matrix,
                seq1,
                seq2,
                matchValue,
                gapValue,
                mismatchValue,
                seq1.length - 1,
                seq2.length - 1
            )
        var ret: Array[(String, String)] = Array.empty[(String, String)]
        if (y == 0 || x == 0)
            var sub1 = seq1.take(y+1)
            var sub2 = seq2.take(x+1)
            if (y > 0)
                sub2 = "-" * (sub1.size - 1) ++ sub2
            else if (x > 0)
                sub1 = "-" * (sub2.size - 1) ++ sub1
            val trace1 = (sub1 +: alignSeq1).mkString
            val trace2 = (sub2 +: alignSeq2).mkString

            val connection = connect(trace1, trace2).mkString
            val maxLen = max(seq1.length, seq2.length)
            val matches = connection.count(_ == '|')

            var out = s"\n$trace1"
            out ++= s"\n$connection"
            out ++= s"\n$trace2\n"
            out ++= s"\nGaps: ${trace1.count(_ == '-') + trace2.count(_ == '-')}"
            out ++= s"\n- Sequence 1: ${trace1.count(_ == '-')}"
            out ++= s"\n- Sequence 2: ${trace2.count(_ == '-')}\n"
            out ++= s"\nMatches: $matches of $maxLen (${"%.2f".format(100 * matches.toDouble / maxLen)}%)\n"

            if file != null then
                file.write(out)
            else
                println(out)

        else
            if (matrix(y+1)(x+1) == matrix(y)(x) + (if (seq1(y) == seq2(x)) matchValue else mismatchValue))
                // Diagonal move (match/mismatch)
                backtrack(file, matrix, seq1, seq2, matchValue, gapValue, mismatchValue, y - 1, x - 1, seq1(y) +: alignSeq1, seq2(x) +: alignSeq2)
            if (matrix(y+1)(x+1) == matrix(y)(x+1) + gapValue)
                // Up move (gap in seq2)
                backtrack(file, matrix, seq1, seq2, matchValue, gapValue, mismatchValue, y - 1, x, seq1(y) +: alignSeq1, '-' +: alignSeq2)
            if (matrix(y+1)(x+1) == matrix(y+1)(x) + gapValue)
                // Left move (gap in seq1)
                backtrack(file, matrix, seq1, seq2, matchValue, gapValue, mismatchValue, y, x - 1, '-' +: alignSeq1, seq2(x) +: alignSeq2)

    def connect(seq1: String, seq2: String): IndexedSeq[String] =
        for (n <- 0 until seq1.length())
            yield if (seq1(n) == seq2(n)) then "|" else " "

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
