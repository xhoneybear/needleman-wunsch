package dev.lisek.needleman_wunsch.plot

import org.nspl.group
import dev.lisek.needleman_wunsch.ZDepth

/** Animate best tracks. */
object Animation:
    var anim: Thread = null

    /**
      * Start the animation.
      *
      * @param n Number of tracks
      * @param onIteration Function to call on each iteration
      */
    def start(n: Int, onIteration: (Int) => Unit) =
        if anim == null || !anim.isAlive() then
            anim = Thread(() =>
                try
                    for i <- 0 until n do
                        onIteration(i)
                        Thread.sleep(1000)
                catch
                    case _: InterruptedException =>
            )
            anim.start()

    /**
      * Stop the animation.
      */
    def stop =
        if anim != null && anim.isAlive() then
            anim.interrupt()

    /**
      * Check if the animation is currently active.
      */
    def isAlive = anim != null && anim.isAlive()
