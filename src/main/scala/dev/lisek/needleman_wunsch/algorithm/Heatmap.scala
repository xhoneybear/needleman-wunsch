package dev.lisek.needleman_wunsch.algorithm

/**
  * Creates a matrix of scores in specific positions.
  *
  * @param seq1 First sequence
  * @param seq2 Second sequence
  * @param matchValue Points to add if characters match
  * @param gapValue Points to add if there is a gap
  * @param mismatchValue Points to add if characters don't match
  * @return Matrix of scores
  */
def heatmap(
    seq1: String,
    seq2: String,
    matchValue: Int,
    gapValue: Int,
    mismatchValue: Int
): Array[Array[Double]] =
    // Initialize the matrix
    var grid = Array.ofDim[Double](seq1.length + 1, seq2.length + 1)

    // Fill in the 0th row and column
    for i <- 0 to seq1.length do
        grid(i)(0) = gapValue * i
    for j <- 0 to seq2.length do
        grid(0)(j) = gapValue * j

    // Iterate over the entire matrix
    for i <- 1 to seq1.length do
        for j <- 1 to seq2.length do

            // Calculate potential scores
            val matchScore = if seq1(i - 1) == seq2(j - 1) then matchValue else mismatchValue
            val score = grid(i - 1)(j - 1) + matchScore
            val gap1 = grid(i)(j - 1) + gapValue
            val gap2 = grid(i - 1)(j) + gapValue

            // Choose the highest score
            grid(i)(j) =
                if (score > gap1 && score > gap2) score
                else if (gap1 > gap2) gap1
                else gap2

    // Return the matrix
    grid
