package dev.lisek.needleman_wunsch.util

import dev.lisek.needleman_wunsch.util.Parser.parse

/**
  * Parsed command-line parameters.
  *
  * @param args Command-line arguments
  */
class Parameters(args: Map[String, Any]):

    // Sequences (with titles) to be compared
    val sequences = args.get("sequences").get.asInstanceOf[List[(String, String)]]

    // Titles
    val title1 = sequences(0)(0)
    val title2 = sequences(1)(0)

    // Sequences
    val seq1 = sequences(0)(1)
    val seq2 = sequences(1)(1)

    // Scores
    val matchValue = args.get("match").get.asInstanceOf[Int]
    val gapValue = args.get("gap").get.asInstanceOf[Int]
    val mismatchValue = args.get("mismatch").get.asInstanceOf[Int]

    // Maximum number of tracks to calculate
    val maxTracks = args.get("tracklimit").get.asInstanceOf[Int]

    // Output file
    val outputFile = args.get("output").get.asInstanceOf[String]

    // Whether to create a graph
    val createGraph = args.get("plot").get.asInstanceOf[Boolean]
