// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import crystal.ViewF
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.components.undo.UndoRegion
import explore.GraphQLSchemas.ObservationDB.Types._
import explore.implicits._
import explore.model.AirMassRange
import explore.model.HourAngleRange
import explore.model.ConstraintSetModel
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ConstraintSet
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.ui.forms.FormInputEV
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.optics.ValidFormatNec
import lucuma.ui.reusability._
import monocle.Lens
import monocle.macros.Lenses
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.elements.label.LabelPointing

import ConstraintsQueries._

final case class ConstraintsPanel(
  id:            ConstraintSet.Id,
  constraintSet: View[ConstraintSetModel]
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

  class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State) = AppCtx.runWithCtx { implicit appCtx =>
      val stateView     = ViewF.fromState[IO]($)
      val constraintSet = props.constraintSet

      UndoRegion[ConstraintSetModel] { undoCtx =>
        val undoViewSet =
          UndoView(props.id, constraintSet, undoCtx.setter)

        def nameView = undoViewSet(ConstraintSetModel.name, UpdateConstraintSet.name)
        def erView   =
          undoViewSet(ConstraintSetModel.elevationRange, UpdateConstraintSet.elevationRange)

        def selectEnum[A: Enumerated: Display](
          label:     String,
          lens:      Lens[ConstraintSetModel, A],
          remoteSet: A => EditConstraintSetInput => EditConstraintSetInput
        ) = {
          val id = label.toLowerCase().replaceAll(" ", "-")
          EnumViewSelect(id = id, value = undoViewSet(lens, remoteSet), label = label)
        }

        def updateElevationRange(ert: ElevationRangeType) =
          ert match {
            case AirMass   => erView.set(state.airMass)
            case HourAngle => erView.set(state.hourAngle)
          }

        Form(as = <.div)(
          ExploreStyles.Grid,
          ExploreStyles.ConstraintsGrid,
          FormInputEV(
            id = "name",
            label = "Name",
            value = nameView,
            validFormat = ValidFormatInput.nonEmptyValidFormat,
            errorClazz = ExploreStyles.InputErrorTooltip,
            errorPointing = LabelPointing.Below
          ),
          <.div(
            ExploreStyles.TwoColumnGrid,
            selectEnum("Image Quality",
                       ConstraintSetModel.imageQuality,
                       UpdateConstraintSet.imageQuality
            ),
            selectEnum("Cloud Extinction",
                       ConstraintSetModel.cloudExtinction,
                       UpdateConstraintSet.cloudExtinction
            ),
            selectEnum("Water Vapor",
                       ConstraintSetModel.waterVapor,
                       UpdateConstraintSet.waterVapor
            ),
            selectEnum("Sky Background",
                       ConstraintSetModel.skyBackground,
                       UpdateConstraintSet.skyBackground
            )
          ),
          <.div(
            ExploreStyles.FlexContainer,
            EnumViewSelect(
              id = "ertype",
              label = "Elevation Range",
              value = stateView
                .zoom(State.rangeType)
                .withOnMod(updateElevationRange),
              clazz = ExploreStyles.ElevationRangePicker
            ),
            ReactFragment(
              FormInputEV(
                id = "minam",
                label = "Min",
                value = stateView
                  .zoom(State.airMass)
                  .zoom(AirMassRange.min)
                  .withOnMod(min => erView.set(state.airMass.copy(min = min))),
                errorClazz = ExploreStyles.InputErrorTooltip,
                errorPointing = LabelPointing.Below,
                validFormat = ValidFormatInput
                  .forRefinedBigDecimal[AirMassRange.Value](airMassErrorMsg)
                  .composeValidFormat(ValidFormatNec.lte(state.airMass.max, "Must be <= Max")),
                changeAuditor = ChangeAuditor.accept.decimal(1),
                clazz = ExploreStyles.ElevationRangeEntry
              ),
              FormInputEV(
                id = "maxam",
                label = "Max",
                value = stateView
                  .zoom(State.airMass)
                  .zoom(AirMassRange.max)
                  .withOnMod(max => erView.set(state.airMass.copy(max = max))),
                errorClazz = ExploreStyles.InputErrorTooltip,
                errorPointing = LabelPointing.Below,
                validFormat = ValidFormatInput
                  .forRefinedBigDecimal[AirMassRange.Value](airMassErrorMsg)
                  .composeValidFormat(
                    ValidFormatNec.gte(state.airMass.min, "Must be >= Min")
                  ),
                changeAuditor = ChangeAuditor.accept.decimal(1),
                clazz = ExploreStyles.ElevationRangeEntry
              )
            ).when(state.rangeType === AirMass),
            ReactFragment(
              FormInputEV(
                id = "minha",
                label = "Min",
                value = stateView
                  .zoom(State.hourAngle)
                  .zoom(HourAngleRange.minHours)
                  .withOnMod(min => erView.set(state.hourAngle.copy(minHours = min))),
                errorClazz = ExploreStyles.InputErrorTooltip,
                errorPointing = LabelPointing.Below,
                validFormat = ValidFormatInput
                  .forRefinedBigDecimal[HourAngleRange.Hour](hourAngleErrorMsg)
                  .composeValidFormat(
                    ValidFormatNec.lte(state.hourAngle.maxHours, "Must be <= Max")
                  ),
                changeAuditor = ChangeAuditor.accept.decimal(1),
                clazz = ExploreStyles.ElevationRangeEntry
              ),
              FormInputEV(
                id = "maxha",
                label = "Max",
                value = stateView
                  .zoom(State.hourAngle)
                  .zoom(HourAngleRange.maxHours)
                  .withOnMod(max => erView.set(state.hourAngle.copy(maxHours = max))),
                errorClazz = ExploreStyles.InputErrorTooltip,
                errorPointing = LabelPointing.Below,
                validFormat = ValidFormatInput
                  .forRefinedBigDecimal[HourAngleRange.Hour](hourAngleErrorMsg)
                  .composeValidFormat(
                    ValidFormatNec.gte(state.hourAngle.minHours, "Must be >= Min")
                  ),
                changeAuditor = ChangeAuditor.accept.decimal(1),
                clazz = ExploreStyles.ElevationRangeEntry
              ),
              <.div(
                ExploreStyles.UnitsLabel,
                "hours"
              )
            ).when(state.rangeType === HourAngle)
          ),
          UndoButtons(constraintSet.get, undoCtx)
        )
      }
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
