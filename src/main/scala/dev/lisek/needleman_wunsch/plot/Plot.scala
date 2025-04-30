package dev.lisek.needleman_wunsch.plot

import dev.lisek.needleman_wunsch.algorithm.Track.tracePath
import dev.lisek.needleman_wunsch.ZDepth
import javax.swing.WindowConstants.EXIT_ON_CLOSE
import org.nspl._ 
import org.nspl.awtrenderer._ 
import org.nspl.data.DataMatrix
import scala.io.StdIn.readInt
import scala.math.max
import dev.lisek.needleman_wunsch.util.StringUtils.resultString

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

        val heatmapParams = par
            .xWidth(width)
            .yHeight(height)
            .xNumTicks(0)
            .yNumTicks(0)
            .xNoTickLabel(true)
            .yNoTickLabel(true)

        var trackParams = par
            .xWidth(width)
            .yHeight(height)
            .xlim(Some(-0.5, density(0).length - 0.5))
            .ylim(Some(-0.5, density.length - 0.5))
            .xgrid(false)
            .ygrid(false)
        if (seq2(0) == seq2(1))
            trackParams = trackParams
                .xnames(xTicks)
                .xLabFontSize((32*cellSize).fts)
        else
            trackParams = trackParams
                .xlab(seq2(0))
                .xLabFontSize(.75.fts)
                
        if (seq1(0) == seq1(1))
            trackParams = trackParams
                .ynames(yTicks)
                .yLabFontSize((32*cellSize).fts)
        else
            trackParams = trackParams
                .ylab(seq1(0))
                .yLabFontSize(.75.fts)

        val heatmap = rasterplot(data)(heatmapParams)
        def track(i: Int) = xyplot(tracePath(tracks(i)(0), tracks(i)(1)) -> line(
            color = Color(32, 48, 64),
            stroke = StrokeConf((8*cellSize).fts)
        ))(trackParams)

        val plot = group(heatmap, xyplot()(trackParams), ZDepth)

        val (frame, update) = show(plot)
        frame.setDefaultCloseOperation(EXIT_ON_CLOSE)

        def updatePlot(i: Int) = update(group(heatmap, track(i), ZDepth))

        println(s"${tracks.size} best tracks found")
        println("Enter track number to display it:")

        while frame.isVisible do
            try
                val i = readInt - 1
                if i == -1 then
                    Animation.start(tracks, updatePlot)
                else if Animation.isAlive then
                    Animation.stop
                else
                    update(group(heatmap, track(i), ZDepth))
                    println(resultString(tracks(i)(0), tracks(i)(1)))
            catch
                case _: NumberFormatException => println("Please enter a number")
                case _: ArrayIndexOutOfBoundsException => println("No such track")
