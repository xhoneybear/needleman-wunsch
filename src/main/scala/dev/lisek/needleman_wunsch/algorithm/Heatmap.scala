package dev.lisek.needleman_wunsch.algorithm

import dev.lisek.needleman_wunsch.util.Parameters

/**
  * Creates a matrix of scores in specific positions.
  *
  * @param par Parsed command-line parameters
  * @return Matrix of scores
  */
def heatmap(par: Parameters): Array[Array[Double]] =
    // Initialize the matrix
    var grid = Array.ofDim[Double](par.seq1.length + 1, par.seq2.length + 1)

    // Fill in the 0th row and column
    for i <- 0 to par.seq1.length do
        grid(i)(0) = par.gapValue * i
    for j <- 0 to par.seq2.length do
        grid(0)(j) = par.gapValue * j

    // Iterate over the entire matrix
    for i <- 1 to par.seq1.length do
        for j <- 1 to par.seq2.length do

            // Calculate potential scores
            val matchScore = if par.seq1(i - 1) == par.seq2(j - 1) then par.matchValue else par.mismatchValue
            val score = grid(i - 1)(j - 1) + par.matchValue
            val gap1 = grid(i)(j - 1) + par.gapValue
            val gap2 = grid(i - 1)(j) + par.gapValue

            // Choose the highest score
            grid(i)(j) =
                if (score > gap1 && score > gap2) score
                else if (gap1 > gap2) gap1
                else gap2

    // Return the matrix
    grid
