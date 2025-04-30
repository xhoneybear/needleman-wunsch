package dev.lisek.needleman_wunsch

import dev.lisek.needleman_wunsch.algorithm.heatmap
import dev.lisek.needleman_wunsch.algorithm.Track.backtrack
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

    // Create score matrix
    val grid = heatmap(
        params.sequences(0)(1),
        params.sequences(1)(1),
        params.matchValue,
        params.gapValue,
        params.mismatchValue
    )

    // Find optimal alignments
    val tracks = backtrack(
        writer,
        grid,
        params.sequences(0),
        params.sequences(1),
        params.matchValue,
        params.gapValue,
        params.mismatchValue,
        params.maxTracks,
        params.createGraph
    )
