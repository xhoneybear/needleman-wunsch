package dev.lisek.needleman_wunsch.algorithm

import dev.lisek.needleman_wunsch.util.Parameters

/**
  * Creates a matrix of scores in specific positions.
  *
  * @param par Parsed command-line parameters
  * @return Matrix of scores
  */
def heatmap(par: Parameters, seq1: String, seq2: String): Array[Array[Double]] =
    // Initialize the matrix
    var grid = Array.ofDim[Double](seq1.length + 1, seq2.length + 1)

    // Fill in the 0th row and column
    for i <- 0 to seq1.length do
        grid(i)(0) = par.gapValue * i
    for j <- 0 to seq2.length do
        grid(0)(j) = par.gapValue * j

    // Iterate over the entire matrix
    for i <- 1 to seq1.length do
        for j <- 1 to seq2.length do

            // Calculate potential scores
            val matchScore = if seq1(i - 1) == seq2(j - 1) then par.matchValue else par.mismatchValue
            val score = grid(i - 1)(j - 1) + matchScore
            val gap1 = grid(i)(j - 1) + par.gapValue
            val gap2 = grid(i - 1)(j) + par.gapValue

            // Choose the highest score
            grid(i)(j) =
                if (score > gap1 && score > gap2) score
                else if (gap1 > gap2) gap1
                else gap2

    // Return the matrix
    grid
