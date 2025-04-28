package dev.lisek.needleman_wunsch.algorithm

def heatmap(
    seq1: String,
    seq2: String,
    matchValue: Int,
    gapValue: Int,
    mismatchValue: Int
): Array[Array[Double]] =
    var grid = Array.ofDim[Double](seq1.length + 1, seq2.length + 1)

    for i <- 0 to seq1.length do
        grid(i)(0) = gapValue * i

    for j <- 0 to seq2.length do
        grid(0)(j) = gapValue * j

    for i <- 1 to seq1.length do
        for j <- 1 to seq2.length do
            val matchScore = if seq1(i - 1) == seq2(j - 1) then matchValue else mismatchValue
            val score = grid(i - 1)(j - 1) + matchScore
            val delete = grid(i - 1)(j) + gapValue
            val insert = grid(i)(j - 1) + gapValue
            grid(i)(j) =
                if (score > delete && score > insert) score
                else if (delete > insert) delete
                else insert
    grid
