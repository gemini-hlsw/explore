// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.svg_<^.*
import lucuma.core.math.*
import lucuma.react.common.*
import lucuma.refined.*

case class OffsetGridDisplay(
  offsets:          List[Offset],
  id:               String = "offset-grid",
  svgSize:          PosInt = 400.refined,
  padding:          PosInt = 40.refined,
  verticalLines:    PosInt = 11.refined,
  horizontalLines:  PosInt = 11.refined,
  showGrid:         Boolean = true,
  showAxes:         Boolean = true,
  showOffsetPoints: Boolean = true,
  showNumbers:      Boolean = false,
  showArrows:       Boolean = true,
  borderLines:      Boolean = true,
  pointRadius:      PosInt = 3.refined,
  svgCss:           Css = OffsetEditorStyles.Svg,
  backgroundCss:    Css = OffsetEditorStyles.Background,
  gridCss:          Css = OffsetEditorStyles.GridLines,
  axesCss:          Css = OffsetEditorStyles.Axes,
  labelsCss:        Css = OffsetEditorStyles.Labels,
  pointsCss:        Css = OffsetEditorStyles.OffsetPoints,
  arrowsCss:        Css = OffsetEditorStyles.Arrows,
  crosshairCss:     Css = OffsetEditorStyles.Crosshair
) extends ReactFnProps[OffsetGridDisplay](OffsetGridDisplay.component)

object OffsetGridDisplay {
  type Props = OffsetGridDisplay

  case class Gridlines(pPositions: List[BigDecimal], qPositions: List[BigDecimal])
  case class OffsetValues(p: BigDecimal, q: BigDecimal, originalOffset: Offset)

  private def calculateRanges(
    pValues:     List[BigDecimal],
    qValues:     List[BigDecimal],
    borderLines: Boolean
  ): (Gridlines, BigDecimal) = {
    val pRange       = pValues.minOption
      .zip(pValues.maxOption)
      .map((min, max) => min.abs.max(max.abs))
    val qRange       = qValues.minOption
      .zip(qValues.maxOption)
      .map((min, max) => min.abs.max(max.abs))
    val dataMaxRange = (pRange, qRange).mapN(_.max(_)).getOrElse(BigDecimal(5.0))

    val (finalAxisExtent, interval) = calculateGridLines(dataMaxRange)

    val pPositions = gridPositions(finalAxisExtent, interval, borderLines)
    val qPositions = gridPositions(finalAxisExtent, interval, borderLines)

    (Gridlines(pPositions, qPositions), finalAxisExtent)
  }

  private def calculateGridLines(dataMaxRange: BigDecimal): (BigDecimal, BigDecimal) =
    val axisExtent = calculateAxisExtent(dataMaxRange)
    (axisExtent, calculateLinesSeparation(axisExtent, 3))

  private def calculateLinesSeparation(
    axisExtent:      BigDecimal,
    minLinesPerSide: Int
  ): BigDecimal =
    val maxInterval = axisExtent / minLinesPerSide

    val candidates = List(1.0, 2.0, 5.0, 10.0, 20.0, 25.0, 50.0, 100.0, 150.0, 200.0, 250.0, 300.0, 500.0)
    candidates.findLast(_ <= maxInterval).getOrElse(candidates.head)

  private def calculateAxisExtent(dataMaxRange: BigDecimal): BigDecimal = {
    val paddedRange = (dataMaxRange * 1.15).max(5.0) // 1.15 padding for tighter fit

    val magnitude  = math.pow(10, math.floor(math.log10(paddedRange.toDouble)))
    val normalized = paddedRange / magnitude

    // Choose a reasonable factor with more granular options
    val factor =
      if (normalized <= 1) 1.0
      else if (normalized <= 1.5) 1.5
      else if (normalized <= 2) 2.0
      else if (normalized <= 2.5) 2.5
      else if (normalized <= 3) 3.0
      else if (normalized <= 4) 4.0
      else if (normalized <= 5) 5.0
      else if (normalized <= 6) 6.0
      else if (normalized <= 8) 8.0
      else 10.0

    BigDecimal(factor * magnitude)
  }

  private def gridPositions(
    axisExtent:    BigDecimal,
    interval:      BigDecimal,
    includeBorder: Boolean
  ): List[BigDecimal] = {
    val steps    = (axisExtent / interval).toInt
    val allLines = (-steps to steps).map(_ * interval).toList

    if (includeBorder) {
      allLines
    } else {
      allLines.filterNot(line => line >= axisExtent - interval * 0.1)
    }
  }

  private def gridLines(
    centerX:      Double,
    centerY:      Double,
    gridSize:     Int,
    padding:      Int,
    gridStrategy: Gridlines,
    maxOffset:    BigDecimal,
    id:           String,
    css:          Css,
    fontSize:     String
  ): VdomNode =
    def verticalLine(offsetValue: BigDecimal, idx: Int) =
      val x = centerX - (offsetValue / maxOffset) * (gridSize / 2)

      <.line(
        ^.key             := s"$id-v-$idx",
        ^.x1              := x.toDouble,
        ^.y1              := padding,
        ^.x2              := x.toDouble,
        ^.y2              := padding + gridSize,
        ^.strokeDasharray := (if (offsetValue.abs < 0.01) "" else "2,2")
      )

    def horizontalLine(offsetValue: BigDecimal, idx: Int) =
      val y = centerY - (offsetValue / maxOffset) * (gridSize / 2)

      <.line(
        ^.key             := s"$id-h-$idx",
        ^.x1              := padding,
        ^.y1              := y.toDouble,
        ^.x2              := padding + gridSize,
        ^.y2              := y.toDouble,
        ^.strokeDasharray := (if (offsetValue.abs < 0.01) "" else "2,2")
      )

    def verticalLineLabel(offsetValue: BigDecimal, idx: Int) =
      val x = centerX - (offsetValue / maxOffset) * (gridSize / 2)

      if (offsetValue.abs > 0.01)
        <.text(
          ^.key        := s"$id-v-label-$idx",
          ^.x          := x.toDouble,
          ^.y          := padding + gridSize + 12,
          ^.textAnchor := "middle",
          ^.fontSize   := fontSize,
          ^.fill       := "currentColor",
          if (offsetValue == offsetValue.rounded) f"${offsetValue.toInt}" else f"$offsetValue%.1f"
        )
      else EmptyVdom

    def horizontalLineLabel(offsetValue: BigDecimal, idx: Int) =
      val y = centerY - (offsetValue / maxOffset) * (gridSize / 2) // Invert Y for SVG
      if (offsetValue.abs > 0.01)
        <.text(
          ^.key        := s"$id-h-label-$idx",
          ^.x          := padding + gridSize + 5,
          ^.y          := y.toDouble + 3,
          ^.textAnchor := "start",
          ^.fontSize   := fontSize,
          ^.fill       := "currentColor",
          if (offsetValue == offsetValue.rounded) f"${offsetValue.toInt}" else f"$offsetValue%.1f"
        )
      else EmptyVdom

    <.g(
      css,
      gridStrategy.pPositions.zipWithIndex.toTagMod(using verticalLine),
      gridStrategy.qPositions.zipWithIndex.toTagMod(using horizontalLine),
      gridStrategy.pPositions.zipWithIndex.toTagMod(using verticalLineLabel),
      gridStrategy.qPositions.zipWithIndex.toTagMod(using horizontalLineLabel)
    )

  private def axes(
    centerX:  Double,
    centerY:  Double,
    gridSize: Int,
    padding:  Int,
    id:       String,
    css:      Css
  ): VdomNode =
    <.g(
      css,
      // P axis (vertical)
      <.line(^.key       := s"$id-p-axis",
             ^.x1 := centerX,
             ^.y1 := padding,
             ^.x2 := centerX,
             ^.y2 := padding + gridSize
      ),
      // Q axis (horizontal)
      <.line(^.key       := s"$id-q-axis",
             ^.x1 := padding,
             ^.y1 := centerY,
             ^.x2 := padding + gridSize,
             ^.y2 := centerY
      ),
      // P axis arrow
      <.polygon(
        ^.key     := s"$id-p-arrow",
        ^.points  := s"${padding - 5},${centerY} ${padding + 5},${centerY - 5} ${padding + 5},${centerY + 5}",
        ^.fill    := "currentColor"
      ),
      // Q axis arrow
      <.polygon(
        ^.key     := s"$id-q-arrow",
        ^.points  := s"${centerX},${padding - 5} ${centerX - 5},${padding + 5} ${centerX + 5},${padding + 5}",
        ^.fill    := "currentColor"
      )
    )

  private def axisLabels(
    gridSize: Int,
    padding:  Int,
    id:       String,
    css:      Css
  ): VdomNode =
    <.g(
      css,
      <.text(
        ^.key        := s"$id-p-label",
        ^.x          := padding - 11,
        ^.y          := padding + gridSize / 2,
        ^.textAnchor := "middle",
        ^.transform  := s"rotate(-90, ${padding - 11}, ${padding + gridSize / 2})",
        "p (arcsec)"
      ),
      <.text(
        ^.key        := s"$id-q-label",
        ^.x          := padding + gridSize / 2,
        ^.y          := padding - 11,
        ^.textAnchor := "middle",
        "q (arcsec)"
      )
    )

  private def offsetArrows(
    offsetValues: List[OffsetValues],
    maxOffset:    BigDecimal,
    centerX:      Double,
    centerY:      Double,
    gridSize:     Int,
    showArrows:   Boolean,
    id:           String,
    css:          Css
  ): VdomNode =
    def arrow(from: OffsetValues, to: OffsetValues, idx: Int) =
      val x1 = centerX - (from.p.toDouble / maxOffset.toDouble) * (gridSize / 2)
      val y1 = centerY - (from.q.toDouble / maxOffset.toDouble) * (gridSize / 2)
      val x2 = centerX - (to.p.toDouble / maxOffset.toDouble) * (gridSize / 2)
      val y2 = centerY - (to.q.toDouble / maxOffset.toDouble) * (gridSize / 2)

      val dx = x2 - x1
      val dy = y2 - y1
      val length = math.sqrt(dx * dx + dy * dy)

      if (length > 0) {
        val scaleFactor = if (offsetValues.length > 50) 0.7 else if (offsetValues.length > 20) 0.8 else 1.0
        val arrowheadLength = 8.0 * scaleFactor
        val arrowheadWidth = 4.0 * scaleFactor

        val unitX = dx / length
        val unitY = dy / length

        val tipOffset = 3.0 * scaleFactor
        val arrowTipX = x2 - unitX * tipOffset // offset from point edge
        val arrowTipY = y2 - unitY * tipOffset

        val arrowBaseX = arrowTipX - unitX * arrowheadLength
        val arrowBaseY = arrowTipY - unitY * arrowheadLength

        val perpX = -unitY * arrowheadWidth
        val perpY = unitX * arrowheadWidth

        React.Fragment(
          <.line(
            ^.key := s"$id-arrow-line-$idx",
            ^.x1 := x1.toDouble,
            ^.y1 := y1.toDouble,
            ^.x2 := arrowBaseX.toDouble,
            ^.y2 := arrowBaseY.toDouble,
            ^.strokeWidth := "1"
          ),
          <.polygon(
            ^.key := s"$id-arrow-head-$idx",
            ^.points := s"$arrowTipX,$arrowTipY ${arrowBaseX + perpX},${arrowBaseY + perpY} ${arrowBaseX - perpX},${arrowBaseY - perpY}"
          )
        )
      } else EmptyVdom

    if (showArrows)
      <.g(
        css,
        offsetValues.zip(offsetValues.drop(1)).zipWithIndex.toTagMod(using { case ((from, to), idx) =>
          arrow(from, to, idx)
        })
      )
    else EmptyVdom

  private def offsetPoints(
    offsetValues: List[OffsetValues],
    maxOffset:    BigDecimal,
    centerX:      Double,
    centerY:      Double,
    gridSize:     Int,
    pointRadius:  Int,
    showNumbers:  Boolean,
    id:           String,
    css:          Css,
    fontSize:     String
  ): VdomNode =
    def offsetPoint(offsetValue: OffsetValues, idx: Int) =
      val p = offsetValue.p.toDouble
      val q = offsetValue.q.toDouble
      val x = centerX - (p / maxOffset) * (gridSize / 2)
      val y = centerY - (q / maxOffset) * (gridSize / 2)

      React.Fragment(
        <.circle(
          ^.key := s"$id-offset-$idx",
          ^.cx  := x.toDouble,
          ^.cy  := y.toDouble,
          ^.r   := pointRadius,
          <.title(f"p: $p%.2f″, q: $q%.2f″")
        ),
        if (showNumbers)
          <.text(
            ^.key      := s"$id-offset-label-$idx",
            ^.x        := x.toDouble + pointRadius + 3,
            ^.y        := y.toDouble - pointRadius - 3,
            ^.fontSize := fontSize,
            ^.fill     := "currentColor",
            idx.toString
          )
        else EmptyVdom
      )

    <.g(
      css,
      offsetValues.zipWithIndex.toTagMod(using offsetPoint)
    )

  val component = ScalaFnComponent[Props]: props =>
    val offsetValues = props.offsets.map(offset =>
      OffsetValues(
        Offset.P.signedDecimalArcseconds.get(offset.p),
        Offset.Q.signedDecimalArcseconds.get(offset.q),
        offset
      )
    )

    val pValues = offsetValues.map(_.p).distinct
    val qValues = offsetValues.map(_.q).distinct

    val (gridStrategy, axisExtent) = calculateRanges(pValues, qValues, props.borderLines)

    // just a heuristic
    val isSmall           = props.svgSize.value < 350
    val showLabels        = props.showAxes && !isSmall
    val gridLabelFontSize = if (isSmall) s"${math.max(7, props.svgSize.value / 40)}px" else "10px"

    val radius  = math.max(1, math.min(3, props.svgSize.value / 150)).toInt
    val padding =
      if (isSmall) math.max(20, props.svgSize.value / 16) else props.padding.value - 5

    val gridSize = props.svgSize.value - 2 * padding
    val centerX  = padding + gridSize / 2.0
    val centerY  = padding + gridSize / 2.0

    <.svg(
      props.svgCss,
      ^.width  := props.svgSize.value,
      ^.height := props.svgSize.value,
      <.rect(
        props.backgroundCss,
        ^.x      := 0,
        ^.y      := 0,
        ^.width  := props.svgSize.value,
        ^.height := props.svgSize.value
      ),
      gridLines(centerX,
                centerY,
                gridSize,
                padding.toInt,
                gridStrategy,
                axisExtent,
                props.id,
                props.gridCss,
                gridLabelFontSize
      ).when(props.showGrid),
      axes(centerX, centerY, gridSize, padding.toInt, props.id, props.axesCss)
        .when(props.showAxes),
      axisLabels(gridSize, padding.toInt, props.id, props.labelsCss)
        .when(showLabels),
      offsetArrows(offsetValues,
                   axisExtent,
                   centerX,
                   centerY,
                   gridSize,
                   props.showArrows,
                   props.id,
                   props.arrowsCss
      ),
      offsetPoints(offsetValues,
                   axisExtent,
                   centerX,
                   centerY,
                   gridSize,
                   radius,
                   props.showNumbers,
                   props.id,
                   props.pointsCss,
                   gridLabelFontSize
      ).when(props.showOffsetPoints)
    )
}
