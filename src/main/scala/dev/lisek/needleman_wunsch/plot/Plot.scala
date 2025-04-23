package dev.lisek.needleman_wunsch.plot

import dev.lisek.needleman_wunsch.algorithm.Track.tracePath
import dev.lisek.needleman_wunsch.ZDepth
import org.nspl._ 
import org.nspl.awtrenderer._ 
import org.nspl.data.DataMatrix
import scala.io.StdIn.readInt
import scala.math.max

object Plot:
    def plot(seq1: (String, String), seq2: (String, String), density: Array[Array[Double]], tracks: Array[(String, String)]): Unit =
        val data = DataMatrix(
            rows = density.flatten,
            numCols = density(0).length,
            numRows = density.length
        )

        val cellSize = 1.0/max(density.length, density(0).length)
        val width = (32*seq2(1).length*cellSize).fts
        val height = (32*seq1(1).length*cellSize).fts
        val xTicks = Seq((.0, "")) :++ (for (i <- 1 to seq2(1).length) yield (i.toDouble, seq2(1)(i-1).toString))
        val yTicks = Seq((.0, "")) :++ (for (i <- 1 to seq1(1).length) yield (i.toDouble, seq1(1)(i-1).toString))
        var par2 = par
            .xWidth(width)
            .yHeight(height)
            .xlim(Some(-0.5, density(0).length - 0.5))
            .ylim(Some(-0.5, density.length - 0.5))
            .xgrid(false)
            .ygrid(false)
        if (seq2(0) == seq2(1))
            par2 = par2
                .xnames(xTicks)
                .xLabFontSize((32*cellSize).fts)
        else
            par2 = par2
                .xlab(seq2(0))
                .xLabFontSize(.75.fts)
                
        if (seq1(0) == seq1(1))
            par2 = par2
                .ynames(yTicks)
                .yLabFontSize((32*cellSize).fts)
        else
            par2 = par2
                .ylab(seq1(0))
                .yLabFontSize(.75.fts)
        
        val plot1 = rasterplot(data)(par
            .xWidth(width)
            .yHeight(height)
            .xNumTicks(0)
            .yNumTicks(0)
            .xNoTickLabel(true)
            .yNoTickLabel(true)
        )
        def plot2(i: Int) = xyplot(tracePath(tracks(i)._1, tracks(i)._2) -> line(
            color = Color(32, 48, 64),
            stroke = StrokeConf((8*cellSize).fts)
        ))(par2)

        val plot = group(plot1, plot2(0), ZDepth)

        val (frame, update) = show(plot)

        println(s"${tracks.size} best tracks found")
        println("Enter track number to display it:")


        while frame.isVisible do
            try
                update(group(plot1, plot2(readInt - 1), ZDepth))
                print("\u033c")
            catch
                case _: NumberFormatException =>
                case _: ArrayIndexOutOfBoundsException =>
