package dev.lisek.needleman_wunsch

import dev.lisek.needleman_wunsch.algorithm.Track.heatmap
import dev.lisek.needleman_wunsch.util.Parser.parse
import dev.lisek.needleman_wunsch.plot.Plot.plot
import dev.lisek.needleman_wunsch.algorithm.Track.backtrack
import dev.lisek.needleman_wunsch.algorithm.Track.tracePath

@main
def main(args: String*) =
    val values = parse(args)

    val sequences = values.get("sequences").get.asInstanceOf[List[(String, String)]]
    val matchValue = values.get("match").get.asInstanceOf[Int]
    val gapValue = values.get("gap").get.asInstanceOf[Int]
    val mismatchValue = values.get("mismatch").get.asInstanceOf[Int]
    val outputFile = values.get("output").get.asInstanceOf[String]
    val createGraph = values.get("plot").get.asInstanceOf[Boolean]

    var file = null: java.io.File
    var writer = null: java.io.FileWriter

    if !outputFile.isBlank() then
        file = new java.io.File(outputFile)
        writer = new java.io.FileWriter(file)

    var out = "--- Evaluation results ---\n"
    out ++= s"\nSequence 1: ${sequences(0)(0)}"
    out ++= s"\nSequence 2: ${sequences(1)(0)}\n"
    out ++= "\nConnections:\n"

    val grid = heatmap(sequences(0)(1), sequences(1)(1), matchValue, gapValue, mismatchValue)
    val tracks = backtrack(writer, grid, sequences(0)(1), sequences(1)(1), matchValue, gapValue, mismatchValue)
    val score = grid(grid.length - 1)(grid(0).length - 1)

    out ++= s"\nSimilarity score: $score"

    if outputFile.isBlank then
        println(out)
    else
        writer.write(out)
        writer.close()

    // if (createGraph)
    //     plot(sequences(0), sequences(1), grid, tracks)
