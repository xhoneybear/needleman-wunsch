package dev.lisek.needleman_wunsch

import dev.lisek.needleman_wunsch.algorithm.heatmap
import dev.lisek.needleman_wunsch.algorithm.Track
import dev.lisek.needleman_wunsch.util.Parser.parse
import dev.lisek.needleman_wunsch.algorithm.CenterStar
import dev.lisek.needleman_wunsch.util.StringUtils.output

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

    var out = "--- Evaluation results ---\n"
    out ++= s"Sequences:\n"
    for seq <- params.sequences do
        out ++= s"  - ${seq._1}\n"
    out ++= s"\nParameters:\n"
    out ++= s"  - Match value: ${params.matchValue}\n"
    out ++= s"  - Mismatch value: ${params.mismatchValue}\n"
    out ++= s"  - Gap value: ${params.gapValue}\n\n"
    if !params.quiet then
        out ++= "Connections:\n\n"

    output(writer, out)

    // Find optimal alignments
    val cs = CenterStar(params, writer)
    val msa = cs.msa

    out = "--- Alignment results ---\n"
    out ++= msa.map(_._2).mkString("\n")
    out ++= "\n\n" + cs.computeStats(msa)

    output(writer, out)

    // Close the FileWriter
    if file != null then
        writer.close
