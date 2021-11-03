// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.common.ConstraintsQueries._
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.AirMassRange
import explore.model.ConstraintSet
import explore.model.ElevationRange
import explore.model.Help
import explore.model.HourAngleRange
import explore.model.reusability._
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.callback.CallbackCats._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.implicits._
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.TruncatedRefinedBigDecimal
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.optics.ValidFormatNec
import lucuma.ui.reusability._
import monocle.Lens
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.elements.label.LabelPointing

final case class ConstraintsPanel(
  obsIds:        List[Observation.Id],
  constraintSet: View[ConstraintSet],
  undoStacks:    View[UndoStacks[IO, ConstraintSet]],
  renderInTitle: Tile.RenderInTitle
) extends ReactProps[ConstraintsPanel](ConstraintsPanel.component)

object ConstraintsPanel {
  type Props = ConstraintsPanel

  val airMassErrorMsg   = NonEmptyString.unsafeFrom(
    f"Must be ${AirMassRange.MinValue.toDouble}%.1f to ${AirMassRange.MaxValue.toDouble}%.1f"
  )
  val hourAngleErrorMsg = NonEmptyString.unsafeFrom(
    f"Must be ${HourAngleRange.MinHour.toDouble}%.1f to ${HourAngleRange.MaxHour.toDouble}%.1f"
  )

  sealed abstract class ElevationRangeType(val label: String) extends Product with Serializable

  object ElevationRangeType {
    case object AirMass   extends ElevationRangeType("Air Mass")
    case object HourAngle extends ElevationRangeType("Hour Angle")

    implicit val enumeratedElevationRangeType: Enumerated[ElevationRangeType] =
      Enumerated.of(AirMass, HourAngle)

    implicit val displayElevationRangeType: Display[ElevationRangeType] =
      Display.byShortName(_.label)
  }

  import ElevationRangeType._

  // State is read-only. Changes are written directly to View received in props, and state is always derived.
  final case class State(
    rangeType: ElevationRangeType,
    airMass:   AirMassRange,
    hourAngle: HourAngleRange
  )

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive
  protected implicit val stateReuse: Reusability[State] = Reusability.derive

  def initialState(props: Props): State = props.constraintSet.get.elevationRange match {
    case am @ AirMassRange(_, _)   => State(ElevationRangeType.AirMass, am, HourAngleRange.Default)
    case ha @ HourAngleRange(_, _) => State(ElevationRangeType.HourAngle, AirMassRange.Default, ha)
  }

  def updateState(props: Props, state: State): State =
    props.constraintSet.get.elevationRange match {
      case am @ AirMassRange(_, _)
          if state.rangeType =!= ElevationRangeType.AirMass | state.airMass =!= am =>
        state.copy(rangeType = ElevationRangeType.AirMass, airMass = am)
      case ha @ HourAngleRange(_, _)
          if state.rangeType =!= ElevationRangeType.HourAngle | state.hourAngle =!= ha =>
        state.copy(rangeType = ElevationRangeType.HourAngle, hourAngle = ha)
      case _ => state
    }

  class Backend() {

    private def renderFn(
      props:        Props,
      state:        State,
      undoCtx:      UndoCtx[ConstraintSet]
    )(implicit ctx: AppContextIO): VdomNode = {
      val undoViewSet = UndoView(props.obsIds, undoCtx)

      val erView =
        undoViewSet(ConstraintSet.elevationRange, UpdateConstraintSet.elevationRange)

      def selectEnum[A: Enumerated: Display](
        label:     String,
        helpId:    Help.Id,
        lens:      Lens[ConstraintSet, A],
        remoteSet: A => EditConstraintSetInput => EditConstraintSetInput
      ) = {
        val id = label.toLowerCase().replaceAll(" ", "-")
        ReactFragment(
          <.label(label, HelpIcon(helpId)),
          EnumViewSelect(id = id, value = undoViewSet(lens, remoteSet))
        )
      }

      val erTypeView: View[ElevationRangeType] =
        View[ElevationRangeType](
          state.rangeType,
          (mod, cb) =>
            erView
              .setCB(
                mod(state.rangeType) match {
                  case AirMass   => state.airMass
                  case HourAngle => state.hourAngle
                },
                _ match {
                  case AirMassRange(_, _)   => cb(AirMass)
                  case HourAngleRange(_, _) => cb(HourAngle)
                }
              )
        )

      val airMassView: View[AirMassRange] =
        View[AirMassRange](
          state.airMass,
          (mod, cb) =>
            erView
              .zoom(ElevationRange.airMass)
              .modCB(mod, _.map(cb).orEmpty)
        )

      val hourAngleView: View[HourAngleRange] =
        View[HourAngleRange](
          state.hourAngle,
          (mod, cb) =>
            erView
              .zoom(ElevationRange.hourAngle)
              .modCB(mod, _.map(cb).orEmpty)
        )

      <.div(
        props.renderInTitle(
          <.span(ExploreStyles.TitleUndoButtons)(UndoButtons(undoCtx))
        ),
        Form(clazz = ExploreStyles.ConstraintsGrid)(
          selectEnum("Image Quality",
                     "constraints/main/iq.md",
                     ConstraintSet.imageQuality,
                     UpdateConstraintSet.imageQuality
          ),
          selectEnum("Cloud Extinction",
                     "constraints/main/ce.md",
                     ConstraintSet.cloudExtinction,
                     UpdateConstraintSet.cloudExtinction
          ),
          selectEnum("Water Vapor",
                     "constraints/main/wv.md",
                     ConstraintSet.waterVapor,
                     UpdateConstraintSet.waterVapor
          ),
          selectEnum("Sky Background",
                     "constraints/main/sb.md",
                     ConstraintSet.skyBackground,
                     UpdateConstraintSet.skyBackground
          ),
          <.label("Elevation Range", HelpIcon("constraints/main/er.md")),
          <.div(
            ExploreStyles.ConstraintsElevationRangeGroup,
            EnumViewSelect(
              id = "ertype",
              value = erTypeView,
              upward = true,
              clazz = ExploreStyles.ElevationRangePicker
            ),
            ReactFragment(
              <.label("Min"),
              FormInputEV(
                id = "minam",
                value = airMassView
                  .zoom(AirMassRange.min)
                  .zoomSplitEpi(
                    TruncatedRefinedBigDecimal.unsafeRefinedBigDecimal[AirMassRange.Value, 1]
                  ),
                errorClazz = ExploreStyles.InputErrorTooltip,
                errorPointing = LabelPointing.Below,
                validFormat = ValidFormatInput
                  .forRefinedTruncatedBigDecimal[AirMassRange.Value, 1](airMassErrorMsg)
                  .andThen(
                    ValidFormatNec.lte(
                      TruncatedRefinedBigDecimal[AirMassRange.Value, 1](
                        state.airMass.max
                      ).get,
                      "Must be <= Max"
                    )
                  ),
                changeAuditor = ChangeAuditor.accept.decimal(1),
                clazz = ExploreStyles.ElevationRangeEntry
              ),
              <.label("Max"),
              FormInputEV(
                id = "maxam",
                value = airMassView
                  .zoom(AirMassRange.max)
                  .zoomSplitEpi(
                    TruncatedRefinedBigDecimal.unsafeRefinedBigDecimal[AirMassRange.Value, 1]
                  ),
                errorClazz = ExploreStyles.InputErrorTooltip,
                errorPointing = LabelPointing.Below,
                validFormat = ValidFormatInput
                  .forRefinedTruncatedBigDecimal[AirMassRange.Value, 1](airMassErrorMsg)
                  .andThen(
                    ValidFormatNec.gte(
                      TruncatedRefinedBigDecimal[AirMassRange.Value, 1](
                        state.airMass.min
                      ).get,
                      "Must be >= Min"
                    )
                  ),
                changeAuditor = ChangeAuditor.accept.decimal(1),
                clazz = ExploreStyles.ElevationRangeEntry
              )
            ).when(state.rangeType === AirMass),
            ReactFragment(
              <.label("Min"),
              FormInputEV(
                id = "minha",
                value = hourAngleView
                  .zoom(HourAngleRange.minHours)
                  .zoomSplitEpi(
                    TruncatedRefinedBigDecimal.unsafeRefinedBigDecimal[HourAngleRange.Hour, 1]
                  ),
                errorClazz = ExploreStyles.InputErrorTooltip,
                errorPointing = LabelPointing.Below,
                validFormat = ValidFormatInput
                  .forRefinedTruncatedBigDecimal[HourAngleRange.Hour, 1](hourAngleErrorMsg)
                  .andThen(
                    ValidFormatNec.lte(TruncatedRefinedBigDecimal[HourAngleRange.Hour, 1](
                                         state.hourAngle.maxHours
                                       ).get,
                                       "Must be <= Max"
                    )
                  ),
                changeAuditor = ChangeAuditor.accept.decimal(1),
                clazz = ExploreStyles.ElevationRangeEntry
              ),
              <.label("Max"),
              FormInputEV(
                id = "maxha",
                value = hourAngleView
                  .zoom(HourAngleRange.maxHours)
                  .zoomSplitEpi(
                    TruncatedRefinedBigDecimal.unsafeRefinedBigDecimal[HourAngleRange.Hour, 1]
                  ),
                errorClazz = ExploreStyles.InputErrorTooltip,
                errorPointing = LabelPointing.Below,
                validFormat = ValidFormatInput
                  .forRefinedTruncatedBigDecimal[HourAngleRange.Hour, 1](hourAngleErrorMsg)
                  .andThen(
                    ValidFormatNec.gte(TruncatedRefinedBigDecimal[HourAngleRange.Hour, 1](
                                         state.hourAngle.minHours
                                       ).get,
                                       "Must be >= Min"
                    )
                  ),
                changeAuditor = ChangeAuditor.accept.decimal(1),
                clazz = ExploreStyles.ElevationRangeEntry
              ),
              <.label("Hours")
            ).when(state.rangeType === HourAngle)
          )
        )
      )
    }

    def render(props: Props, state: State) = AppCtx.using { implicit appCtx =>
      renderFn(props, state, UndoContext(props.undoStacks, props.constraintSet))
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .getDerivedStateFromPropsAndState[State] { (props, stateOpt) =>
        stateOpt match {
          case Some(state) => updateState(props, state)
          case None        => initialState(props)
        }
      }
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build
}
