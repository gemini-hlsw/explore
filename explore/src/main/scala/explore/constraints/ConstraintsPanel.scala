// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.effect.IO
import cats.syntax.all._
import crystal.react.View
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import explore.common.ConstraintsQueries._
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.Help
import explore.model.display._
import explore.undo.UndoContext
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Observation
import lucuma.core.model.validation.ModelValidators
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.validation._
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.reusability._
import monocle.Lens
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.elements.label.LabelPointing
import lucuma.refined.*

final case class ConstraintsPanel(
  obsIds:           List[Observation.Id],
  constraintSet:    View[ConstraintSet],
  undoStacks:       View[UndoStacks[IO, ConstraintSet]],
  renderInTitle:    Tile.RenderInTitle
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ConstraintsPanel](ConstraintsPanel.component)

object ConstraintsPanel {
  protected type Props = ConstraintsPanel

  private sealed abstract class ElevationRangeType(val label: String)
      extends Product
      with Serializable

  private object ElevationRangeType {
    case object AirMass   extends ElevationRangeType("Air Mass")
    case object HourAngle extends ElevationRangeType("Hour Angle")

    implicit val enumeratedElevationRangeType: Enumerated[ElevationRangeType] =
      Enumerated.of(AirMass, HourAngle)

    implicit val displayElevationRangeType: Display[ElevationRangeType] =
      Display.byShortName(_.label)
  }

  import ElevationRangeType._

  private final case class ElevationRangeOptions(
    rangeType: ElevationRangeType,
    airMass:   ElevationRange.AirMass,
    hourAngle: ElevationRange.HourAngle
  ) {
    def toElevationRange(er: ElevationRange): ElevationRangeOptions =
      er match {
        case am @ ElevationRange.AirMass(_, _)   =>
          copy(rangeType = ElevationRangeType.AirMass, airMass = am)
        case ha @ ElevationRange.HourAngle(_, _) =>
          copy(rangeType = ElevationRangeType.HourAngle, hourAngle = ha)
      }
  }

  private object ElevationRangeOptions {
    def fromElevationRange(er: ElevationRange): ElevationRangeOptions =
      ElevationRangeOptions(
        ElevationRangeType.AirMass,
        ElevationRange.AirMass.Default,
        ElevationRange.HourAngle.Default
      ).toElevationRange(er)
  }

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateBy(props =>
        ElevationRangeOptions.fromElevationRange(props.constraintSet.get.elevationRange)
      )
      .useEffectWithDepsBy((props, _) => props.constraintSet.get.elevationRange)(
        (_, elevationRangeOptions) =>
          elevationRange => elevationRangeOptions.modState(_.toElevationRange(elevationRange))
      )
      .render { (props, elevationRangeOptions) =>
        implicit val ctx = props.ctx

        val undoCtx: UndoContext[ConstraintSet] = UndoContext(props.undoStacks, props.constraintSet)

        val undoViewSet = UndoView(props.obsIds, undoCtx)

        val erView =
          undoViewSet(ConstraintSet.elevationRange, UpdateConstraintSet.elevationRange)

        def selectEnum[A: Enumerated: Display](
          label:     String,
          helpId:    Help.Id,
          lens:      Lens[ConstraintSet, A],
          remoteSet: A => ConstraintSetInput => ConstraintSetInput
        ) = {
          val id = label.toLowerCase().replaceAll(" ", "-")
          ReactFragment(
            <.label(label, HelpIcon(helpId)),
            EnumViewSelect(id = id, value = undoViewSet(lens, remoteSet))
          )
        }

        val erTypeView: View[ElevationRangeType] =
          View[ElevationRangeType](
            elevationRangeOptions.value.rangeType,
            (mod, cb) =>
              erView
                .setCB(
                  mod(elevationRangeOptions.value.rangeType) match {
                    case AirMass   => elevationRangeOptions.value.airMass
                    case HourAngle => elevationRangeOptions.value.hourAngle
                  },
                  _ match {
                    case ElevationRange.AirMass(_, _)   => cb(AirMass)
                    case ElevationRange.HourAngle(_, _) => cb(HourAngle)
                  }
                )
          )

        val airMassView: View[ElevationRange.AirMass] =
          View[ElevationRange.AirMass](
            elevationRangeOptions.value.airMass,
            (mod, cb) =>
              erView
                .zoom(ElevationRange.airMass)
                .modCB(mod, _.map(cb).orEmpty)
          )

        val hourAngleView: View[ElevationRange.HourAngle] =
          View[ElevationRange.HourAngle](
            elevationRangeOptions.value.hourAngle,
            (mod, cb) =>
              erView
                .zoom(ElevationRange.hourAngle)
                .modCB(mod, _.map(cb).orEmpty)
          )

        React.Fragment(
          props.renderInTitle(
            <.span(ExploreStyles.TitleUndoButtons)(UndoButtons(undoCtx))
          ),
          Form(clazz = ExploreStyles.ConstraintsGrid)(
            selectEnum(
              "Image Quality",
              "constraints/main/iq.md".refined,
              ConstraintSet.imageQuality,
              UpdateConstraintSet.imageQuality
            ),
            selectEnum(
              "Cloud Extinction",
              "constraints/main/ce.md".refined,
              ConstraintSet.cloudExtinction,
              UpdateConstraintSet.cloudExtinction
            ),
            selectEnum(
              "Water Vapor",
              "constraints/main/wv.md".refined,
              ConstraintSet.waterVapor,
              UpdateConstraintSet.waterVapor
            ),
            selectEnum(
              "Sky Background",
              "constraints/main/sb.md".refined,
              ConstraintSet.skyBackground,
              UpdateConstraintSet.skyBackground
            ),
            <.label("Elevation Range", HelpIcon("constraints/main/er.md".refined)),
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
                  id = "minam".refined,
                  value = airMassView.zoom(ElevationRange.AirMass.min),
                  errorClazz = ExploreStyles.InputErrorTooltip,
                  errorPointing = LabelPointing.Below,
                  validFormat = ModelValidators.airMassElevationRangeValidWedge.andThen(
                    ValidSplitEpiNec.lte(elevationRangeOptions.value.airMass.max,
                                         "Must be <= Max".refined
                    )
                  ),
                  changeAuditor = ChangeAuditor.accept.decimal(1.refined),
                  clazz = ExploreStyles.ElevationRangeEntry
                ),
                <.label("Max"),
                FormInputEV(
                  id = "maxam".refined,
                  value = airMassView.zoom(ElevationRange.AirMass.max),
                  errorClazz = ExploreStyles.InputErrorTooltip,
                  errorPointing = LabelPointing.Below,
                  validFormat = ModelValidators.airMassElevationRangeValidWedge.andThen(
                    ValidSplitEpiNec.gte(elevationRangeOptions.value.airMass.min,
                                         "Must be >= Min".refined
                    )
                  ),
                  changeAuditor = ChangeAuditor.accept.decimal(1.refined),
                  clazz = ExploreStyles.ElevationRangeEntry
                )
              ).when(elevationRangeOptions.value.rangeType === AirMass),
              ReactFragment(
                <.label("Min"),
                FormInputEV(
                  id = "minha".refined,
                  value = hourAngleView.zoom(ElevationRange.HourAngle.minHours),
                  errorClazz = ExploreStyles.InputErrorTooltip,
                  errorPointing = LabelPointing.Below,
                  validFormat = ModelValidators.hourAngleElevationRangeValidWedge.andThen(
                    ValidSplitEpiNec.lte(
                      elevationRangeOptions.value.hourAngle.maxHours,
                      "Must be <= Max".refined
                    )
                  ),
                  changeAuditor = ChangeAuditor.accept.decimal(1.refined),
                  clazz = ExploreStyles.ElevationRangeEntry
                ),
                <.label("Max"),
                FormInputEV(
                  id = "maxha".refined,
                  value = hourAngleView.zoom(ElevationRange.HourAngle.maxHours),
                  errorClazz = ExploreStyles.InputErrorTooltip,
                  errorPointing = LabelPointing.Below,
                  validFormat = ModelValidators.hourAngleElevationRangeValidWedge.andThen(
                    ValidSplitEpiNec.gte(
                      elevationRangeOptions.value.hourAngle.minHours,
                      "Must be >= Min".refined
                    )
                  ),
                  changeAuditor = ChangeAuditor.accept.decimal(1.refined),
                  clazz = ExploreStyles.ElevationRangeEntry
                ),
                <.label("Hours")
              ).when(elevationRangeOptions.value.rangeType === HourAngle)
            )
          )
        )
      }
}
