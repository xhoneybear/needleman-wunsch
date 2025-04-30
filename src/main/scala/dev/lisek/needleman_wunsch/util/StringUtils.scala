package dev.lisek.needleman_wunsch.util

import scala.math.max
import java.io.FileWriter

/** Utility functions for string handling. */
object StringUtils:
    /**
      * Creates a result string.
      *
      * @param seq1 First sequence title
      * @param seq2 Second sequence title
      * @return Result string
      */
    def resultString(
        seq1: String,
        seq2: String
    ): String =
        val connection = connect(seq1, seq2).mkString
        val matches = connection.count(_ == '|')
        val maxLen = max(seq1.length, seq2.length)

        var out = s"$seq1"
        out ++= s"\n$connection"
        out ++= s"\n$seq2\n"
        out ++= s"\nGaps: ${seq1.count(_ == '-') + seq2.count(_ == '-')}"
        out ++= s"\n- Sequence 1: ${seq1.count(_ == '-')}"
        out ++= s"\n- Sequence 2: ${seq2.count(_ == '-')}\n"
        out ++= s"\nMatches: $matches of $maxLen (${"%.2f".format(100 * matches.toDouble / maxLen)}%)\n\n"
        out

    /**
      * Creates a connection string.
      *
      * @param seq1 First sequence
      * @param seq2 Second sequence
      * @return Connection string
      */
    def connect(seq1: String, seq2: String): IndexedSeq[String] =
        for (n <- 0 until seq1.length())
            yield if (seq1(n) == seq2(n)) then "|" else " "

    /**
      * Outputs a string to a file or console.
      *
      * @param file FileWriter to write to
      * @param string String to write
      */
    def output(
        file: FileWriter,
        string: String
    ): Unit =
        if file != null then
            file.write(string)
        else
            println(string)
