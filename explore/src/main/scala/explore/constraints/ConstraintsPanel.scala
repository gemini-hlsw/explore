// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.common.ConstraintsQueries._
import explore.common.ObsQueries._
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.AirMassRange
import explore.model.Help
import explore.model.HourAngleRange
import explore.model.reusability._
import explore.schemas.ObservationDB.Types._
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.implicits._
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.TruncatedRefinedBigDecimal
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.optics.ValidFormatNec
import lucuma.ui.reusability._
import monocle.Lens
import monocle.macros.Lenses
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.elements.label.Label
import react.semanticui.elements.label.LabelPointing

final case class ConstraintsPanel(
  obsId:         Observation.Id,
  constraintSet: View[ConstraintSetData],
  undoStacks:    View[UndoStacks[IO, ConstraintSetData]],
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

  @Lenses
  final case class State(
    rangeType: ElevationRangeType,
    airMass:   AirMassRange,
    hourAngle: HourAngleRange,
    copying:   Boolean = false
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

  class Backend($ : BackendScope[Props, State]) {

    private def renderFn(
      props:        Props,
      state:        View[State],
      undoCtx:      UndoCtx[ConstraintSetData]
    )(implicit ctx: AppContextIO): VdomNode = {
      val undoViewSet = UndoView(props.obsId, undoCtx)

      def nameView = undoViewSet(ConstraintSetData.name, UpdateConstraintSet.name)
      def erView   =
        undoViewSet(ConstraintSetData.elevationRange, UpdateConstraintSet.elevationRange)

      def selectEnum[A: Enumerated: Display](
        label:     String,
        helpId:    Help.Id,
        lens:      Lens[ConstraintSetData, A],
        remoteSet: A => EditConstraintSetInput => EditConstraintSetInput
      ) = {
        val id = label.toLowerCase().replaceAll(" ", "-")
        EnumViewSelect(id = id,
                       value = undoViewSet(lens, remoteSet),
                       label = Label(label, HelpIcon(helpId))
        )
      }

      def updateElevationRange(ert: ElevationRangeType) =
        ert match {
          case AirMass   => erView.set(state.get.airMass)
          case HourAngle => erView.set(state.get.hourAngle)
        }

      <.div(
        props.renderInTitle(
          <.span(ExploreStyles.TitleStrip)(
            UndoButtons(undoCtx)
          )
        ),
        Form(loading = state.get.copying)(ExploreStyles.Grid, ExploreStyles.ConstraintsGrid)(
          FormInputEV(
            id = "name",
            label = Label("Name", HelpIcon("constraints/main/name.md")),
            clazz = ExploreStyles.ConstraintsNameField,
            value = nameView,
            validFormat = ValidFormatInput.nonEmptyValidFormat,
            errorClazz = ExploreStyles.InputErrorTooltip,
            errorPointing = LabelPointing.Below
          ),
          selectEnum("Image Quality",
                     "constraints/main/iq.md",
                     ConstraintSetData.imageQuality,
                     UpdateConstraintSet.imageQuality
          ),
          selectEnum("Cloud Extinction",
                     "constraints/main/ce.md",
                     ConstraintSetData.cloudExtinction,
                     UpdateConstraintSet.cloudExtinction
          ),
          selectEnum("Water Vapor",
                     "constraints/main/wv.md",
                     ConstraintSetData.waterVapor,
                     UpdateConstraintSet.waterVapor
          ),
          selectEnum("Sky Background",
                     "constraints/main/sb.md",
                     ConstraintSetData.skyBackground,
                     UpdateConstraintSet.skyBackground
          ),
          <.div(
            ExploreStyles.FlexContainer,
            ExploreStyles.ConstraintsElevationRangeGroup,
            EnumViewSelect(
              id = "ertype",
              label = Label("Elevation Range", HelpIcon("constraints/main/er.md")),
              value = state
                .zoom(State.rangeType)
                .withOnMod(updateElevationRange),
              clazz = ExploreStyles.ElevationRangePicker
            ),
            ReactFragment(
              FormInputEV(
                id = "minam",
                label = "Min",
                value = state
                  .zoom(State.airMass)
                  .zoom(AirMassRange.min)
                  .zoomSplitEpi(
                    TruncatedRefinedBigDecimal.unsafeRefinedBigDecimal[AirMassRange.Value, 1]
                  )
                  .withOnMod(min => erView.set(state.get.airMass.copy(min = min.value))),
                errorClazz = ExploreStyles.InputErrorTooltip,
                errorPointing = LabelPointing.Below,
                validFormat = ValidFormatInput
                  .forRefinedTruncatedBigDecimal[AirMassRange.Value, 1](airMassErrorMsg)
                  .composeValidFormat(
                    ValidFormatNec.lte(
                      TruncatedRefinedBigDecimal[AirMassRange.Value, 1](
                        state.get.airMass.max
                      ).get,
                      "Must be <= Max"
                    )
                  ),
                changeAuditor = ChangeAuditor.accept.decimal(1),
                clazz = ExploreStyles.ElevationRangeEntry
              ),
              FormInputEV(
                id = "maxam",
                label = "Max",
                value = state
                  .zoom(State.airMass)
                  .zoom(AirMassRange.max)
                  .zoomSplitEpi(
                    TruncatedRefinedBigDecimal.unsafeRefinedBigDecimal[AirMassRange.Value, 1]
                  )
                  .withOnMod(max => erView.set(state.get.airMass.copy(max = max.value))),
                errorClazz = ExploreStyles.InputErrorTooltip,
                errorPointing = LabelPointing.Below,
                validFormat = ValidFormatInput
                  .forRefinedTruncatedBigDecimal[AirMassRange.Value, 1](airMassErrorMsg)
                  .composeValidFormat(
                    ValidFormatNec.gte(
                      TruncatedRefinedBigDecimal[AirMassRange.Value, 1](
                        state.get.airMass.min
                      ).get,
                      "Must be >= Min"
                    )
                  ),
                changeAuditor = ChangeAuditor.accept.decimal(1),
                clazz = ExploreStyles.ElevationRangeEntry
              )
            ).when(state.get.rangeType === AirMass),
            ReactFragment(
              FormInputEV(
                id = "minha",
                label = "Min",
                value = state
                  .zoom(State.hourAngle)
                  .zoom(HourAngleRange.minHours)
                  .zoomSplitEpi(
                    TruncatedRefinedBigDecimal.unsafeRefinedBigDecimal[HourAngleRange.Hour, 1]
                  )
                  .withOnMod(min => erView.set(state.get.hourAngle.copy(minHours = min.value))),
                errorClazz = ExploreStyles.InputErrorTooltip,
                errorPointing = LabelPointing.Below,
                validFormat = ValidFormatInput
                  .forRefinedTruncatedBigDecimal[HourAngleRange.Hour, 1](hourAngleErrorMsg)
                  .composeValidFormat(
                    ValidFormatNec.lte(TruncatedRefinedBigDecimal[HourAngleRange.Hour, 1](
                                         state.get.hourAngle.maxHours
                                       ).get,
                                       "Must be <= Max"
                    )
                  ),
                changeAuditor = ChangeAuditor.accept.decimal(1),
                clazz = ExploreStyles.ElevationRangeEntry
              ),
              FormInputEV(
                id = "maxha",
                label = "Max",
                value = state
                  .zoom(State.hourAngle)
                  .zoom(HourAngleRange.maxHours)
                  .zoomSplitEpi(
                    TruncatedRefinedBigDecimal.unsafeRefinedBigDecimal[HourAngleRange.Hour, 1]
                  )
                  .withOnMod(max => erView.set(state.get.hourAngle.copy(maxHours = max.value))),
                errorClazz = ExploreStyles.InputErrorTooltip,
                errorPointing = LabelPointing.Below,
                validFormat = ValidFormatInput
                  .forRefinedTruncatedBigDecimal[HourAngleRange.Hour, 1](hourAngleErrorMsg)
                  .composeValidFormat(
                    ValidFormatNec.gte(TruncatedRefinedBigDecimal[HourAngleRange.Hour, 1](
                                         state.get.hourAngle.minHours
                                       ).get,
                                       "Must be >= Min"
                    )
                  ),
                changeAuditor = ChangeAuditor.accept.decimal(1),
                clazz = ExploreStyles.ElevationRangeEntry
              ),
              <.div(ExploreStyles.UnitsLabel, "hours")
            ).when(state.get.rangeType === HourAngle)
          )
        )
      )
    }

    def render(props: Props) = AppCtx.using { implicit appCtx =>
      renderFn(props, ViewF.fromStateSyncIO($), UndoContext(props.undoStacks, props.constraintSet))
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
