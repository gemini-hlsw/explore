// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.data.Nested
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.cats.refTypeEq
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import explore.model.ExploreModelValidators.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.geom.OffsetGenerator.*
import lucuma.core.math.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.common.*
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.react.primereact.Button
import lucuma.ui.reusability.given
import monocle.Focus
import monocle.Lens
import org.typelevel.log4cats.Logger

case class OffsetEditor(
  offsets:    View[Option[List[Offset]]],
  onUpdate:   List[Offset] => Callback,
  pointCount: PosInt
)(using L: Logger[IO])
    extends ReactFnProps[OffsetEditor](OffsetEditor.component):
  given Logger[IO] = L

  def hasOffsets = offsets.get.exists(_.nonEmpty)

object OffsetEditor {
  type Props = OffsetEditor

  // Reusability instances
  given Reusability[GridType]          = Reusability.byRef
  given Reusability[RectangularParams] = Reusability.byRef
  given Reusability[SpiralParams]      = Reusability.byRef
  given Reusability[RandomParams]      = Reusability.byRef

  sealed trait GridParams:
    def gridType: GridType
    def shift: Offset
    def rotation: Angle

  case class RectangularParams(
    rows:     PosInt,
    cols:     PosInt,
    stepP:    Angle,
    stepQ:    Angle,
    shift:    Offset = Offset.Zero,
    rotation: Angle = Angle.Angle0
  ) extends GridParams:
    def gridType: GridType = GridType.Rectangular

  object RectangularParams:
    val rows: Lens[RectangularParams, PosInt]    = Focus[RectangularParams](_.rows)
    val cols: Lens[RectangularParams, PosInt]    = Focus[RectangularParams](_.cols)
    val stepP: Lens[RectangularParams, Angle]    = Focus[RectangularParams](_.stepP)
    val stepQ: Lens[RectangularParams, Angle]    = Focus[RectangularParams](_.stepQ)
    val shift: Lens[RectangularParams, Offset]   = Focus[RectangularParams](_.shift)
    val rotation: Lens[RectangularParams, Angle] = Focus[RectangularParams](_.rotation)

    val initial: RectangularParams = RectangularParams(
      rows = 1.refined,
      cols = 1.refined,
      stepP = Angle.fromDoubleArcseconds(5.0),
      stepQ = Angle.fromDoubleArcseconds(5.0),
      shift = Offset.Zero,
      rotation = Angle.Angle0
    )

  case class SpiralParams(
    size:     Angle,
    shift:    Offset = Offset.Zero,
    rotation: Angle = Angle.Angle0
  ) extends GridParams:
    def gridType: GridType = GridType.Spiral

  object SpiralParams:
    val size: Lens[SpiralParams, Angle]     = Focus[SpiralParams](_.size)
    val shift: Lens[SpiralParams, Offset]   = Focus[SpiralParams](_.shift)
    val rotation: Lens[SpiralParams, Angle] = Focus[SpiralParams](_.rotation)

    val initial = SpiralParams(
      size = Angle.fromDoubleArcseconds(10.0),
      shift = Offset.Zero,
      rotation = Angle.Angle0
    )

  case class RandomParams(
    size:     Angle, // area radius
    shift:    Offset = Offset.Zero,
    rotation: Angle = Angle.Angle0
  ) extends GridParams:
    def gridType: GridType = GridType.Random

  object RandomParams:
    val size: Lens[RandomParams, Angle]     = Focus[RandomParams](_.size)
    val shift: Lens[RandomParams, Offset]   = Focus[RandomParams](_.shift)
    val rotation: Lens[RandomParams, Angle] = Focus[RandomParams](_.rotation)
    val initial                             = RandomParams(
      size = Angle.fromDoubleArcseconds(10.0),
      shift = Offset.Zero,
      rotation = Angle.Angle0
    )

  enum GridType(val tag: String) derives Enumerated:
    case Rectangular extends GridType("rectangular")
    case Spiral      extends GridType("spiral")
    case Random      extends GridType("random")

  given Display[GridType] = Display.byShortName[GridType](_.tag)

  private def calculateSquareGridDimensions(pointCount: PosInt): (PosInt, PosInt) = {
    val count       = pointCount.value
    val side        = math.ceil(math.sqrt(count.toDouble)).toInt
    val refinedSide = PosInt.unsafeFrom(side)
    (refinedSide, refinedSide)
  }

  private def generateCurrentGrid(
    gridType:      GridType,
    rectParams:    RectangularParams,
    spiralParams:  SpiralParams,
    randomParams:  RandomParams,
    pointCount:    PosInt,
    updatePreview: List[Offset] => Callback
  )(using Logger[IO]): Callback = {
    val currentParams = gridType match {
      case GridType.Rectangular => rectParams
      case GridType.Spiral      => spiralParams
      case GridType.Random      => randomParams
    }

    currentParams match {
      case r: RectangularParams =>
        val (rows, cols) = calculateSquareGridDimensions(pointCount)
        val newGrid      = grid(rows, cols, r.stepP, r.stepQ)
        val rotated      = if (r.rotation != Angle.Angle0) newGrid.map(_.rotate(r.rotation)) else newGrid
        val shifted      = rotated.map(_ + r.shift)
        updatePreview(shifted.toList)

      case s: SpiralParams =>
        val fallbackGrid = Nested(spiral[IO](pointCount, s.size))
        val rotated      =
          if (s.rotation != Angle.Angle0) fallbackGrid.map(_.rotate(s.rotation))
          else fallbackGrid
        val shifted      = rotated.map(_ + s.shift)
        shifted.value.flatMap(o => updatePreview(o.toList).to[IO]).runAsync

      case r: RandomParams =>
        val fallbackGrid = Nested(random[IO](pointCount, r.size))
        val rotated      =
          if (r.rotation != Angle.Angle0) fallbackGrid.map(_.rotate(r.rotation)) else fallbackGrid
        val shifted      = rotated.map(_ + r.shift)
        shifted.value.flatMap(o => updatePreview(o.toList).to[IO]).runAsync
    }
  }

  val component = ScalaFnComponent[Props]: props =>
    import props.given

    props.offsets.get.foreach: o =>
      o.zipWithIndex.foreach: (o, i) =>
        val (p, q) = Offset.signedDecimalArcseconds.get(o)
        println(s"i: $i = p: $p, q: $q")

    for {
      // Store complete parameter objects using useStateView
      rectParams     <- useStateView(RectangularParams.initial)
      spiralParams   <- useStateView(SpiralParams.initial)
      randomParams   <- useStateView(RandomParams.initial)
      gridType       <- useStateView(GridType.Random)
      previewOffsets <- useState(props.offsets.get.orElse(None))
      isInitialMount <- useState(true)

      // Combined callback that updates both preview and parent
      updateBoth = (offsets: List[Offset]) => previewOffsets.setState(offsets.some) *> props.onUpdate(offsets)
      // _ <- useEffectWithDeps(props.pointCount): pointCount =>
      //        val (rows, cols) = calculateSquareGridDimensions(pointCount)
      //        rectParams.mod(_.copy(rows = rows, cols = cols))
      _              <- useEffectOnMount {
                          // Only generate if no stored offsets exist
                          generateCurrentGrid(gridType.get,
                                              rectParams.get,
                                              spiralParams.get,
                                              randomParams.get,
                                              props.pointCount,
                                              updateBoth
                          ).unless_(props.hasOffsets)
                        }
      _              <-
        useEffectWithDeps((gridType.get, rectParams.get, spiralParams.get, randomParams.get)):
          (t, r, s, n) =>
            if (isInitialMount.value && props.hasOffsets) {
              // Skip generation on initial mount if we have stored offsets
              isInitialMount.setState(false)
            } else {
              isInitialMount.setState(false) *>
                Callback.log("refresh") *>
                generateCurrentGrid(t,
                                    r,
                                    s,
                                    n,
                                    props.pointCount,
                                    updateBoth
                )
            }
    } yield <.div(
      OffsetEditorStyles.Content,
      <.div(
        OffsetEditorStyles.GridDisplay,
        <.div(OffsetEditorStyles.Container, previewOffsets.value.map(OffsetGridDisplay(_)))
      ),
      <.div(
        OffsetEditorStyles.GridControls,
        <.h4("Generation Parameters"),
        React.Fragment(
          <.label(^.htmlFor := "grid-type", "Grid Type"),
          FormEnumDropdownView(
            id = "grid-type".refined,
            value = gridType,
            placeholder = "Select grid type"
          )
        ),
        gridType.get match {
          case GridType.Rectangular =>
            ReactFragment(
              <.label("Grid Dimensions"),
              <.div(
                s"${rectParams.get.rows.value} × ${rectParams.get.cols.value} (${props.pointCount.value} points based on exposures)"
              ),
              <.div(
                LucumaPrimeStyles.FormField,
                <.label(^.htmlFor := "rect-step-p", "Step P (arcsec)"),
                FormInputTextView(
                  id = "rect-step-p".refined,
                  value = rectParams.zoom(RectangularParams.stepP),
                  validFormat = decimalArcsecondsValidWedge,
                  placeholder = "0.0"
                )
              ),
              <.div(
                LucumaPrimeStyles.FormField,
                <.label(^.htmlFor := "rect-step-q", "Step Q (arcsec)"),
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
                LucumaPrimeStyles.FormField,
                <.label(s"Offsets Count: ${props.pointCount.value}")
              ),
              <.div(
                LucumaPrimeStyles.FormField,
                <.label(^.htmlFor := "spiral-size", "Size (arcsec)"),
                FormInputTextView(
                  id = "spiral-size".refined,
                  value = spiralParams.zoom(SpiralParams.size),
                  validFormat = decimalArcsecondsValidWedge,
                  placeholder = "0.0"
                )
              )
            )
          case GridType.Random      =>
            ReactFragment(
              <.label(s"Offsets Count: ${props.pointCount.value}"),
              <.label(^.htmlFor := "random-size", "Size (arcsec)"),
              FormInputTextView(
                id = "random-size".refined,
                value = randomParams.zoom(RandomParams.size),
                validFormat = decimalArcsecondsValidWedge,
                placeholder = "0.0"
              )
            )
        },
        <.div(
          LucumaPrimeStyles.FormField,
          Button(
            label = "Regenerate Grid",
            severity = Button.Severity.Secondary,
            onClick = generateCurrentGrid(gridType.get,
                                          rectParams.get,
                                          spiralParams.get,
                                          randomParams.get,
                                          props.pointCount,
                                          updateBoth
                      )
          )
        )
      )
    )
}
