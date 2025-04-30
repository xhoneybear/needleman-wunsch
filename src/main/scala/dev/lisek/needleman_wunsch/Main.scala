package dev.lisek.needleman_wunsch

import dev.lisek.needleman_wunsch.algorithm.heatmap
import dev.lisek.needleman_wunsch.algorithm.Track
import dev.lisek.needleman_wunsch.util.Parser.parse

/**
  * Run the Needleman-Wunsch algorithm.
  *
  * @param args Command-line arguments
  */
@main
def main(args: String*) =
    // Parse arguments
    val params = parse(args)

    // Set up output (stdout)
    var file = null: java.io.File
    var writer = null: java.io.FileWriter

    // Set up output (file)
    if !params.outputFile.isBlank() then
        file = new java.io.File(params.outputFile)
        writer = new java.io.FileWriter(file)

    // Find optimal alignments
    val tracks = Track(params, writer).backtrack
