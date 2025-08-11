// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.types.numeric.PosInt
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.ExploreModelValidators.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.geom.OffsetGenerator.*
import lucuma.core.math.*
import lucuma.core.util.Display
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.Checkbox
import lucuma.react.resizeDetector.hooks.*
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import org.typelevel.log4cats.Logger

case class OffsetEditor(
  offsets:     View[List[Offset]],
  onUpdate:    List[Offset] => Callback,
  pointCount:  PosInt,
  defaultSize: Angle
)(using L: Logger[IO])
    extends ReactFnProps[OffsetEditor](OffsetEditor.component):
  given Logger[IO] = L

  def hasOffsets = offsets.get.nonEmpty

object OffsetEditor {
  type Props = OffsetEditor

  given Reusability[GridParams] = Reusability.byEq

  given Display[GridType] = Display.byShortName[GridType](_.tag.capitalize)

  private def squareGridDimension(pointCount: PosInt): PosInt =
    val count = pointCount.value
    val side  = math.ceil(math.sqrt(count.toDouble)).toInt
    PosInt.from(side).getOrElse(1.refined)

  private def generateCurrentGrid(
    currentParams: GridParams,
    pointCount:    PosInt,
    updatePreview: List[Offset] => Callback
  )(using Logger[IO]): Callback =
    currentParams match {
      case r: RectangularParams =>
        val dim     = squareGridDimension(pointCount)
        val newGrid = grid(dim, dim, r.stepP, r.stepQ)
        updatePreview(newGrid.toList)

      case s: SpiralParams =>
        spiral[IO](pointCount, s.size)
          .flatMap(offsets => updatePreview(offsets.toList).to[IO])
          .runAsync

      case r: RandomParams =>
        random[IO](pointCount, r.size)
          .flatMap(offsets => updatePreview(offsets.toList).to[IO])
          .runAsync
    }

  val component = ScalaFnComponent[Props]: props =>
    import props.given

    val dim = squareGridDimension(props.pointCount)

    for {
      rectParams     <- useStateView(RectangularParams(dim, dim, props.defaultSize, props.defaultSize))
      spiralParams   <- useStateView(SpiralParams(props.defaultSize))
      randomParams   <- useStateView(RandomParams(props.defaultSize))
      gridType       <- useStateView(GridType.Random)
      previewOffsets <- useState(props.offsets.get.some)
      showNumbers    <- useStateView(false)
      isInitialMount <- useState(true)
      params          = gridType.get match {
                          case GridType.Rectangular => rectParams.get
                          case GridType.Spiral      => spiralParams.get
                          case GridType.Random      => randomParams.get
                        }
      updateOffsets   =
        (offsets: List[Offset]) => previewOffsets.setState(offsets.some) *> props.onUpdate(offsets)
      _              <- useEffectOnMount {
                          generateCurrentGrid(params, props.pointCount, updateOffsets)
                            .unless_(props.hasOffsets)
                        }
      _              <- useEffectWithDeps(params): params =>
                          isInitialMount.setState(false) *>
                            generateCurrentGrid(params, props.pointCount, updateOffsets)
                              .unless_(isInitialMount.value && props.hasOffsets)
      _              <- useEffectWithDeps(props.pointCount): pointCount =>
                          val dim = squareGridDimension(pointCount)
                          rectParams.mod(_.copy(rows = dim, cols = dim))
      resize         <- useResizeDetector
    } yield
      val size = (resize.width, resize.height).mapN(_.min(_)).map(PosInt.from).flatMap(_.toOption)
      <.div(
        OffsetEditorStyles.Content,
        <.div(
          OffsetEditorStyles.GridDisplay,
          (previewOffsets.value, size).mapN((o, s) =>
            OffsetGridDisplay(o, svgSize = s, showNumbers = showNumbers.get || o.size < 30)
          )
        ).withRef(resize.ref),
        <.div(
          OffsetEditorStyles.GridControls,
          <.h4("Generator Parameters"),
          <.div(
            OffsetEditorStyles.FormRow,
            <.label(^.htmlFor := "grid-type", "Type:"),
            FormEnumDropdownView(
              id = "grid-type".refined,
              value = gridType,
              placeholder = "Select grid type"
            )
          ),
          gridType.get match {
            case GridType.Rectangular =>
              ReactFragment(
                <.div(
                  OffsetEditorStyles.FormRow,
                  <.label("Dimensions:"),
                  <.div(
                    s"${rectParams.get.rows.value} × ${rectParams.get.cols.value} (for ${props.pointCount.value} steps)"
                  )
                ),
                <.div(
                  OffsetEditorStyles.FormRow,
                  <.label(^.htmlFor := "rect-step-p", "p step (arcsec):"),
                  FormInputTextView(
                    id = "rect-step-p".refined,
                    value = rectParams.zoom(RectangularParams.stepP),
                    validFormat = decimalArcsecondsValidWedge,
                    placeholder = "0.0"
                  )
                ),
                <.div(
                  OffsetEditorStyles.FormRow,
                  <.label(^.htmlFor := "rect-step-q", "q step (arcsec):"),
                  FormInputTextView(
                    id = "rect-step-q".refined,
                    value = rectParams.zoom(RectangularParams.stepQ),
                    validFormat = decimalArcsecondsValidWedge,
                    placeholder = "0.0"
                  )
                )
              )
            case GridType.Spiral      =>
              ReactFragment(
                <.div(
                  OffsetEditorStyles.FormRow,
                  <.label(^.htmlFor := "spiral-size", "Size (arcsec):"),
                  FormInputTextView(
                    id = "spiral-size".refined,
                    value = spiralParams.zoom(SpiralParams.size),
                    validFormat = decimalArcsecondsValidWedge,
                    placeholder = "0.0"
                  )
                ),
                <.div(
                  OffsetEditorStyles.FormRow,
                  <.label(s"Count:"),
                  <.label(ExploreStyles.OffsetsCount, props.pointCount.value)
                )
              )
            case GridType.Random      =>
              ReactFragment(
                <.div(
                  OffsetEditorStyles.FormRow,
                  <.label(^.htmlFor := "random-size", "Size (arcsec):"),
                  FormInputTextView(
                    id = "random-size".refined,
                    value = randomParams.zoom(RandomParams.size),
                    validFormat = decimalArcsecondsValidWedge,
                    placeholder = "0.0"
                  )
                ),
                <.div(
                  OffsetEditorStyles.FormRow,
                  <.label(s"Count:"),
                  <.label(ExploreStyles.OffsetsCount, props.pointCount.value)
                )
              )
          },
          <.div(
            OffsetEditorStyles.FormRow,
            <.div(
              Checkbox(
                inputId = "show-numbers",
                checked = showNumbers.get || previewOffsets.value.exists(_.size < 30),
                onChange = checked => showNumbers.set(checked)
              ),
              <.label(^.htmlFor := "show-numbers", " Offset numbers")
            ),
            gridType.get match {
              case GridType.Rectangular => EmptyVdom
              case _                    =>
                Button(
                  text = false,
                  icon = Icons.ArrowsRepeat,
                  severity = Button.Severity.Success,
                  clazz = ExploreStyles.OffsetRegenerate,
                  onClick = generateCurrentGrid(params, props.pointCount, updateOffsets)
                ).mini.compact
            }
          )
        )
      )
}
