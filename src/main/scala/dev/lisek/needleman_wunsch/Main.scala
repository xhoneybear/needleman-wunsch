package dev.lisek.needleman_wunsch

import dev.lisek.needleman_wunsch.algorithm.heatmap
import dev.lisek.needleman_wunsch.algorithm.Track.backtrack
import dev.lisek.needleman_wunsch.algorithm.Track.tracePath
import dev.lisek.needleman_wunsch.util.Parser.parse
import dev.lisek.needleman_wunsch.util.Parameters

@main
def main(args: String*) =
    val params = parse(args)
    var file = null: java.io.File
    var writer = null: java.io.FileWriter

    if !params.outputFile.isBlank() then
        file = new java.io.File(params.outputFile)
        writer = new java.io.FileWriter(file)

    val grid = heatmap(
        params.sequences(0)(1),
        params.sequences(1)(1),
        params.matchValue,
        params.gapValue,
        params.mismatchValue
    )
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
