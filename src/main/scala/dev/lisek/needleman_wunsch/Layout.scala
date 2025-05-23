package dev.lisek.needleman_wunsch

import org.nspl._
import org.nspl.Align._

/**
  * Modified LayoutHelper from nspl.
  * Adjusted for plot stacking.
  */
object Layout:
  /**
    * Calculates the outline of a set of bounds.
    *
    * @param members1 Bounds
    * @param anchor Anchor
    * @return Outline
    */
  def outline(
      members1: Iterator[Bounds],
      anchor: Option[Point]
  ) = {
    var empty = true
    var minX = Double.MaxValue
    var minY = Double.MaxValue
    var maxX = Double.MinValue
    var maxY = Double.MinValue

    members1.foreach { t =>
      if (t.w > 0 || t.h > 0) {
        empty = false
        if (t.x < minX) {
          minX = t.x
        }
        if (t.maxX > maxX) {
          maxX = t.maxX
        }
        if (t.y < minY) {
          minY = t.y
        }
        if (t.maxY > maxY) {
          maxY = t.maxY
        }
      }
    }

    if (empty) Bounds(0, 0, 0, 0, anchor)
    else {

      val w = maxX - minX
      val h = maxY - minY
      Bounds(minX, minY, w, h, anchor)
    }
  }

  /**
    * Transposes a matrix.
    *
    * @param a Matrix to transpose
    * @return Transposed matrix
    */
  def transpose[A](a: Seq[Seq[A]]) = {
    if (a.isEmpty) a
    else {
      val max = a.map(_.size).max
      val uniform =
        a.map { aa =>
          aa ++ List.fill(max - aa.size)(null.asInstanceOf[A])
        }
      uniform.transpose.map(_.filterNot(_ == null))
    }
  }

  /**
    * Modified anchor alignment function from nspl.
    * Combines row and column alignment functions and allows stacking.
    *
    * @param table Boundary table
    * @param horizontalGap Horizontal gap
    * @param verticalGap Vertical gap
    * @param alignRows Whether to align rows
    * @param alignCols Whether to align columns
    * @return
    */
  def alignToAnchors[F: FC](
      table: Seq[Seq[Bounds]],
      horizontalGap: RelFontSize,
      verticalGap: RelFontSize,
      alignRows: Boolean,
      alignCols: Boolean
  ) = {

    val horiz = HorizontalStack(Anchor, horizontalGap)
    val vertic = VerticalStack(Anchor, verticalGap)

    val rowTable = if (alignCols) transpose(table) else table
    val rows = rowTable.map(i => horiz.apply(i))
    val rowOutlines = rows.map(s => outline(s.iterator, anchor = None))
    val rowOutlines_moved = vertic.apply(rowOutlines)
    val yCoordinates = rows zip rowOutlines_moved zip rowOutlines flatMap {
      case ((row, rowbound), rowOutline) =>
        val diff = rowbound.y - rowOutline.y
        row.map { r =>
          r.y + diff
        }
    }

    val colTable = if (alignRows) transpose(table) else table
    val cols = colTable.map(i => vertic.apply(i))
    val columnOutlines = cols.map(s => outline(s.iterator, anchor = None))
    val columnOutlines_moved = horiz.apply(columnOutlines)
    val xCoordinates = cols zip columnOutlines_moved zip columnOutlines map {
      case ((col, colbound), colOutline) =>
        val diff = colbound.x - colOutline.x
        col.map { c =>
          c.x + diff
        }
    }

    (transpose(xCoordinates).flatten zip yCoordinates zip table.flatten).map {
      case ((c, r), b) =>
        Bounds(c, r, b.w, b.h, b.anchor)
    }
  }

/** A Layout which stacks elements on top of each other.*/
object ZDepth extends Layout {
  def apply[F: FC](s: Seq[Bounds]) = {
    if (s.isEmpty) s
    else Layout.alignToAnchors(Seq(s), 0.fts, 0.fts, false, false)
  }
}
