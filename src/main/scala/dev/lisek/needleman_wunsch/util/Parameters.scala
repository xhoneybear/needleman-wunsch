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

    // Scores
    val matchValue = args.get("match").get.asInstanceOf[Int]
    val gapValue = args.get("gap").get.asInstanceOf[Int]
    val mismatchValue = args.get("mismatch").get.asInstanceOf[Int]

    // Output file
    val outputFile = args.get("output").get.asInstanceOf[String]

    // Maximum number of tracks to calculate
    var maxTracks = args.get("tracklimit").get.asInstanceOf[Int]

    // Whether to print connections
    var quiet = args.get("quiet").get.asInstanceOf[Boolean]

    // Whether to create a graph
    var createGraph = args.get("plot").get.asInstanceOf[Boolean]
