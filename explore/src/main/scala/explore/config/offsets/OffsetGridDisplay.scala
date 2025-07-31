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
  showCrosshair:    Boolean = false,
  showBorderLines:  Boolean = false,
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

  case class GridStrategy(
    pPositions: List[Double],
    qPositions: List[Double]
  )

  private def analyzeOffsetPattern(
    pValues: List[Double],
    qValues: List[Double],
    showBorderLines: Boolean
  ): (GridStrategy, Double) = {
    // Debug: Log the input data
    println(s"Grid calculation - pValues: $pValues")
    println(s"Grid calculation - qValues: $qValues")

    // Calculate data range
    val pRange = (pValues.minOption, pValues.maxOption).mapN((min, max) => math.max(math.abs(min), math.abs(max)))
    val qRange = (qValues.minOption, qValues.maxOption).mapN((min, max) => math.max(math.abs(min), math.abs(max)))
    val dataMaxRange = (pRange, qRange).mapN(_.max(_)).getOrElse(5.0)

    println(s"Grid calculation - pRange: $pRange, qRange: $qRange, dataMaxRange: $dataMaxRange")

    // Calculate nice axis extent and interval, ensuring minimum 3 lines per side
    val (finalAxisExtent, interval) = calculateOptimalGridParams(dataMaxRange)

    println(s"Grid calculation - finalAxisExtent: $finalAxisExtent, interval: $interval")

    // Generate grid positions using the final extent
    val pPositions = generateNiceGrid(finalAxisExtent, interval, showBorderLines)
    val qPositions = generateNiceGrid(finalAxisExtent, interval, showBorderLines)

    (GridStrategy(pPositions, qPositions), finalAxisExtent)
  }

  private def calculateOptimalGridParams(dataMaxRange: Double): (Double, Double) = {
    println(s"=== calculateOptimalGridParams called with dataMaxRange=$dataMaxRange ===")

    // Start with initial nice axis extent
    val axisExtent = calculateNiceAxisExtent(dataMaxRange)

    // Calculate interval that ensures at least 3 lines per side
    val interval = calculateIntervalForMinimumLines(axisExtent, 3)

    println(s"Final: axisExtent=$axisExtent, interval=$interval, linesPerSide=${(axisExtent / interval).toInt}")

    (axisExtent, interval)
  }

  private def calculateIntervalForMinimumLines(axisExtent: Double, minLinesPerSide: Int): Double = {
    // Calculate maximum interval that gives us at least minLinesPerSide
    val maxInterval = axisExtent / minLinesPerSide

    println(s"calculateIntervalForMinimumLines: axisExtent=$axisExtent, minLines=$minLinesPerSide, maxInterval=$maxInterval")

    // Find the largest nice interval that is <= maxInterval
    val candidates = List(1.0, 2.0, 5.0, 10.0, 20.0, 50.0, 100.0, 200.0, 500.0)

    val interval = candidates.findLast(_ <= maxInterval).getOrElse(candidates.head)

    println(s"calculateIntervalForMinimumLines: selected interval=$interval, gives ${(axisExtent / interval).toInt} lines per side")

    interval
  }

  private def calculateNiceAxisExtent(dataMaxRange: Double): Double = {
    val paddedRange = math.max(dataMaxRange * 1.3, 5.0) // 30% padding, minimum 5 arcseconds

    // Find the order of magnitude (power of 10)
    val magnitude = math.pow(10, math.floor(math.log10(paddedRange)))

    // Calculate the normalized value (between 1 and 10)
    val normalized = paddedRange / magnitude

    // Choose the appropriate nice factor using 1-2-5-10 pattern
    val niceFactor = if (normalized <= 1) 1
                    else if (normalized <= 2) 2
                    else if (normalized <= 5) 5
                    else 10

    // Calculate the nice extent
    niceFactor * magnitude
  }

  private def calculateNiceInterval(axisExtent: Double): Double = {
    // Target: 3 lines per side (7 total including center)
    // So we want: axisExtent / interval ≈ 3
    val targetInterval = axisExtent / 3.0

    println(s"calculateNiceInterval: axisExtent=$axisExtent, targetInterval=$targetInterval")

    // Find the order of magnitude
    val magnitude = math.pow(10, math.floor(math.log10(targetInterval)))

    // Normalize to 1-10 range
    val normalized = targetInterval / magnitude

    println(s"calculateNiceInterval: magnitude=$magnitude, normalized=$normalized")

    // Choose nice factor using 1-2-5-10 pattern
    // Adjusted thresholds to favor smaller intervals (more grid lines)
    val niceFactor = if (normalized <= 1.2) 1
                    else if (normalized <= 2.5) 2
                    else if (normalized <= 6.0) 5
                    else 10

    val result = niceFactor * magnitude
    println(s"calculateNiceInterval: niceFactor=$niceFactor, result=$result")

    result
  }

  private def generateNiceGrid(axisExtent: Double, interval: Double, includeBorder: Boolean): List[Double] = {
    // Generate symmetric grid from -extent to +extent at nice intervals
    val steps = (axisExtent / interval).toInt
    val allLines = (-steps to steps).map(_ * interval).toList

    if (includeBorder) {
      allLines
    } else {
      // Filter out the extreme border lines (first and last)
      allLines.filterNot(line => math.abs(line) >= axisExtent - interval * 0.1)
    }
  }

  private def gridLines(
    centerX:      Double,
    centerY:      Double,
    gridSize:     Int,
    padding:      Int,
    gridStrategy: GridStrategy,
    maxOffset:    Double,
    id:           String,
    css:          Css
  ): VdomNode =
    def verticalLine(offsetValue: Double, idx: Int) =
      val x             = centerX - (offsetValue / maxOffset) * (gridSize / 2) // Flip P axis for Gemini
      <.line(
        ^.key             := s"$id-v-$idx",
        ^.x1              := x,
        ^.y1              := padding,
        ^.x2              := x,
        ^.y2              := padding + gridSize,
        ^.strokeDasharray := (if (math.abs(offsetValue) < 0.01) "" else "2,2")
      )

    def horizontalLine(offsetValue: Double, idx: Int) =
      val y             = centerY - (offsetValue / maxOffset) * (gridSize / 2) // Invert Y for SVG
      <.line(
        ^.key             := s"$id-h-$idx",
        ^.x1              := padding,
        ^.y1              := y,
        ^.x2              := padding + gridSize,
        ^.y2              := y,
        ^.strokeDasharray := (if (math.abs(offsetValue) < 0.01) "" else "2,2")
      )

    def verticalLineLabel(offsetValue: Double, idx: Int) =
      val x = centerX - (offsetValue / maxOffset) * (gridSize / 2) // Flip P axis for Gemini
      if (math.abs(offsetValue) > 0.01) // Don't label the center line
        <.text(
          ^.key := s"$id-v-label-$idx",
          ^.x := x,
          ^.y := padding + gridSize + 12, // Position at bottom
          ^.textAnchor := "middle",
          ^.fontSize := "10px",
          ^.fill := "currentColor",
          if (offsetValue == offsetValue.round) f"${offsetValue.round}" else f"$offsetValue%.1f"
        )
      else EmptyVdom

    def horizontalLineLabel(offsetValue: Double, idx: Int) =
      val y = centerY - (offsetValue / maxOffset) * (gridSize / 2) // Invert Y for SVG
      if (math.abs(offsetValue) > 0.01) // Don't label the center line
        <.text(
          ^.key := s"$id-h-label-$idx",
          ^.x := padding + gridSize + 5, // Position at right side
          ^.y := y + 3,
          ^.textAnchor := "start",
          ^.fontSize := "10px",
          ^.fill := "currentColor",
          if (offsetValue == offsetValue.round) f"${offsetValue.round}" else f"$offsetValue%.1f"
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
      // P axis arrow (pointing left since positive P grows left)
      <.polygon(
        ^.key    := s"$id-p-arrow",
        ^.points := s"${padding - 5},${centerY} ${padding + 5},${centerY - 5} ${padding + 5},${centerY + 5}",
        ^.fill   := "currentColor"
      ),
      // Q axis arrow (pointing up since positive Q grows up)
      <.polygon(
        ^.key    := s"$id-q-arrow",
        ^.points := s"${centerX},${padding - 5} ${centerX - 5},${padding + 5} ${centerX + 5},${padding + 5}",
        ^.fill   := "currentColor"
      )
    )

  private def axisLabels(
    axisExtent: Double,
    gridSize:   Int,
    padding:    Int,
    id:         String,
    css:        Css
  ): VdomNode =
    <.g(
      css,
      // P axis label (left side since positive P grows left, rotated)
      <.text(
        ^.key        := s"$id-p-label",
        ^.x          := 15,
        ^.y          := padding + gridSize / 2,
        ^.textAnchor := "middle",
        ^.transform  := s"rotate(-90, 15, ${padding + gridSize / 2})",
        "p (arcsec)"
      ),
      // Q axis label
      <.text(
        ^.key        := s"$id-q-label",
        ^.x          := padding + gridSize / 2,
        ^.y          := padding - 15,
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
    maxOffset:   Double,
    centerX:     Double,
    centerY:     Double,
    gridSize:    Int,
    pointRadius: Int,
    id:          String,
    css:         Css
  ): VdomNode =
    def offsetPoint(offset: Offset, idx: Int) =
      val p = Offset.P.signedDecimalArcseconds.get(offset.p).toDouble
      val q = Offset.Q.signedDecimalArcseconds.get(offset.q).toDouble
      val x = centerX - (p / maxOffset) * (gridSize / 2) // Flip P axis for Gemini
      val y = centerY - (q / maxOffset) * (gridSize / 2) // Invert Y for SVG

      React.Fragment(
        <.circle(
          ^.key := s"$id-offset-$idx",
          ^.cx  := x,
          ^.cy  := y,
          ^.r   := pointRadius,
          <.title(f"p: $p%.2f″, q: $q%.2f″")
        ),
        <.text(
          ^.key := s"$id-offset-label-$idx",
          ^.x := x + pointRadius + 3,
          ^.y := y - pointRadius - 3,
          ^.fontSize := "10px",
          ^.fill := "currentColor",
          idx.toString
        )
      )

    <.g(
      css,
      offsets.toList.zipWithIndex.toTagMod(using offsetPoint)
    )

  val component = ScalaFnComponent[Props]: props =>
    // Debug: Log the offsets received by the component
    println(s"OffsetGridDisplay received ${props.offsets.length} offsets:")
    props.offsets.zipWithIndex.foreach { (offset, idx) =>
      val p = Offset.P.signedDecimalArcseconds.get(offset.p).toDouble
      val q = Offset.Q.signedDecimalArcseconds.get(offset.q).toDouble
      println(s"  Offset $idx: p=$p, q=$q")
    }

    // Extract offset values once for both analysis and scaling
    val pValues = props.offsets
      .map(o => Offset.P.signedDecimalArcseconds.get(o.p).toDouble)
      .toList
      .distinct
      .sorted
    val qValues = props.offsets
      .map(o => Offset.Q.signedDecimalArcseconds.get(o.q).toDouble)
      .toList
      .distinct
      .sorted

    // Analyze pattern and calculate display range
    val (gridStrategy, axisExtent) = analyzeOffsetPattern(pValues, qValues, props.showBorderLines)

    val gridSize = props.svgSize.value - 2 * props.padding.value
    val centerX  = props.padding.value + gridSize / 2.0
    val centerY  = props.padding.value + gridSize / 2.0

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
                props.padding.value,
                gridStrategy,
                axisExtent,
                props.id,
                props.gridCss
              ).when(props.showGrid),
      axes(centerX, centerY, gridSize, props.padding.value, props.id, props.axesCss)
        .when(props.showAxes),
      axisLabels(axisExtent, gridSize, props.padding.value, props.id, props.labelsCss)
        .when(props.showAxes),
      offsetPoints(props.offsets,
                   axisExtent,
                   centerX,
                   centerY,
                   gridSize,
                   props.pointRadius.value,
                   props.id,
                   props.pointsCss
      ).when(props.showOffsetPoints),
      centerCrosshair(centerX, centerY, props.id, props.crosshairCss).when(props.showCrosshair)
    )
}
