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
  showCrosshair:    Boolean = false,
  showBorderLines:  Boolean = true,
  pointRadius:      PosInt = 3.refined,
  svgCss:           Css = OffsetEditorStyles.Svg,
  backgroundCss:    Css = OffsetEditorStyles.Background,
  gridCss:          Css = OffsetEditorStyles.GridLines,
  axesCss:          Css = OffsetEditorStyles.Axes,
  labelsCss:        Css = OffsetEditorStyles.Labels,
  pointsCss:        Css = OffsetEditorStyles.OffsetPoints,
  crosshairCss:     Css = OffsetEditorStyles.Crosshair
) extends ReactFnProps[OffsetGridDisplay](OffsetGridDisplay.component)

object OffsetGridDisplay {
  type Props = OffsetGridDisplay

  case class Gridlines(
    pPositions: List[BigDecimal],
    qPositions: List[BigDecimal]
  )

  private def calculateRanges(
    pValues:         List[BigDecimal],
    qValues:         List[BigDecimal],
    showBorderLines: Boolean
  ): (Gridlines, BigDecimal) = {
    val pRange       = (pValues.minOption, pValues.maxOption).mapN((min, max) => min.abs.max(max.abs))
    val qRange       = (qValues.minOption, qValues.maxOption).mapN((min, max) => min.abs.max(max.abs))
    val dataMaxRange = (pRange, qRange).mapN(_.max(_)).getOrElse(BigDecimal(5.0))

    val (finalAxisExtent, interval) = calculateGridLines(dataMaxRange)

    val pPositions = gridPositions(finalAxisExtent, interval, showBorderLines)
    val qPositions = gridPositions(finalAxisExtent, interval, showBorderLines)

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

    val candidates = List(1.0, 2.0, 5.0, 10.0, 20.0, 50.0, 100.0, 200.0, 500.0)
    candidates.findLast(_ <= maxInterval).getOrElse(candidates.head)

  private def calculateAxisExtent(dataMaxRange: BigDecimal): BigDecimal = {
    val paddedRange = (dataMaxRange * 1.3).max(5.0) // 30% padding, minimum 5 arcseconds

    val magnitude  = math.pow(10, math.floor(math.log10(paddedRange.toDouble)))
    val normalized = paddedRange / magnitude

    // Choose a reasonable factor
    val factor =
      if (normalized <= 1) 1
      else if (normalized <= 2) 2
      else if (normalized <= 5) 5
      else 10

    factor * magnitude
  }

  private def gridPositions(
    axisExtent:    BigDecimal,
    interval:      BigDecimal,
    includeBorder: Boolean
  ): List[BigDecimal] = {
    // Generate symmetric grid from -extent to +extent at nice intervals
    val steps    = (axisExtent / interval).toInt
    val allLines = (-steps to steps).map(_ * interval).toList

    if (includeBorder) {
      allLines
    } else {
      // Filter out the extreme border lines (first and last)
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
          ^.y          := padding + gridSize + 12, // Position at bottom
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
          ^.x          := padding + gridSize + 5, // Position at right side
          ^.y          := y.toDouble + 3,
          ^.textAnchor := "start",
          ^.fontSize   := fontSize,
          ^.fill       := "currentColor",
          if (offsetValue == offsetValue.rounded) f"${offsetValue.toInt}" else f"$offsetValue%.1f"
        )
      else EmptyVdom

    <.g(
      css,
      // Vertical grid lines
      gridStrategy.pPositions.zipWithIndex.toTagMod(using verticalLine),
      // Horizontal grid lines
      gridStrategy.qPositions.zipWithIndex.toTagMod(using horizontalLine),
      // Grid line labels
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
      // P axis label (left side since positive P grows left, rotated)
      <.text(
        ^.key        := s"$id-p-label",
        ^.x          := padding - 11,
        ^.y          := padding + gridSize / 2,
        ^.textAnchor := "middle",
        ^.transform  := s"rotate(-90, ${padding - 11}, ${padding + gridSize / 2})",
        "p (arcsec)"
      ),
      // Q axis label
      <.text(
        ^.key        := s"$id-q-label",
        ^.x          := padding + gridSize / 2,
        ^.y          := padding - 11,
        ^.textAnchor := "middle",
        "q (arcsec)"
      )
    )

  private def centerCrosshair(centerX: Double, centerY: Double, id: String, css: Css): VdomNode =
    <.g(
      css,
      <.line(
        ^.key := s"$id-crosshair-h",
        ^.x1  := centerX - 10,
        ^.y1  := centerY,
        ^.x2  := centerX + 10,
        ^.y2  := centerY
      ),
      <.line(
        ^.key := s"$id-crosshair-v",
        ^.x1  := centerX,
        ^.y1  := centerY - 10,
        ^.x2  := centerX,
        ^.y2  := centerY + 10
      )
    )

  private def offsetPoints(
    offsets:     List[Offset],
    maxOffset:   BigDecimal,
    centerX:     Double,
    centerY:     Double,
    gridSize:    Int,
    pointRadius: Int,
    showNumbers: Boolean,
    id:          String,
    css:         Css,
    fontSize:    String
  ): VdomNode =
    def offsetPoint(offset: Offset, idx: Int) =
      val p = Offset.P.signedDecimalArcseconds.get(offset.p).toDouble
      val q = Offset.Q.signedDecimalArcseconds.get(offset.q).toDouble
      val x = centerX - (p / maxOffset) * (gridSize / 2) // Flip P axis for Gemini
      val y = centerY - (q / maxOffset) * (gridSize / 2) // Invert Y for SVG

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
      offsets.toList.zipWithIndex.toTagMod(using offsetPoint)
    )

  val component = ScalaFnComponent[Props]: props =>
    val pValues = props.offsets
      .map(o => Offset.P.signedDecimalArcseconds.get(o.p))
      .toList
      .distinct
      .sorted
    val qValues = props.offsets
      .map(o => Offset.Q.signedDecimalArcseconds.get(o.q))
      .toList
      .distinct
      .sorted

    val (gridStrategy, axisExtent) = calculateRanges(pValues, qValues, props.showBorderLines)

    val isSmall           = props.svgSize.value < 350
    val dynamicPadding    =
      if (isSmall) math.max(20, props.svgSize.value / 16) else props.padding.value - 5
    val showLabels        = props.showAxes && !isSmall
    val gridLabelFontSize = if (isSmall) s"${math.max(7, props.svgSize.value / 40)}px" else "10px"

    val dynamicRadius = math.max(1, math.min(3, props.svgSize.value / 150)).toInt

    val gridSize = props.svgSize.value - 2 * dynamicPadding
    val centerX  = dynamicPadding + gridSize / 2.0
    val centerY  = dynamicPadding + gridSize / 2.0

    <.svg(
      props.svgCss,
      ^.width  := props.svgSize.value,
      ^.height := props.svgSize.value,
      // Background
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
                dynamicPadding.toInt,
                gridStrategy,
                axisExtent,
                props.id,
                props.gridCss,
                gridLabelFontSize
      ).when(props.showGrid),
      axes(centerX, centerY, gridSize, dynamicPadding.toInt, props.id, props.axesCss)
        .when(props.showAxes),
      axisLabels(gridSize, dynamicPadding.toInt, props.id, props.labelsCss)
        .when(showLabels),
      offsetPoints(props.offsets,
                   axisExtent,
                   centerX,
                   centerY,
                   gridSize,
                   dynamicRadius,
                   props.showNumbers,
                   props.id,
                   props.pointsCss,
                   gridLabelFontSize
      ).when(props.showOffsetPoints),
      centerCrosshair(centerX, centerY, props.id, props.crosshairCss).when(props.showCrosshair)
    )
}
