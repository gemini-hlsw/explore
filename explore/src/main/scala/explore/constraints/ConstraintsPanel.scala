// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.ConstraintsQueries
import explore.common.ConstraintsQueries.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Help
import explore.model.ObsIdSet
import explore.undo.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.validation.ModelValidators
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.validation.*
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.EnumDropdownView
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.given
import monocle.Lens

case class ConstraintsPanel(
  obsIds:   ObsIdSet,
  undoCtx:  UndoSetter[ConstraintSet],
  readonly: Boolean
) extends ReactFnProps(ConstraintsPanel.component):
  val constraintSet: ConstraintSet = undoCtx.get

object ConstraintsPanel:
  private type Props = ConstraintsPanel

  private enum ElevationRangeType(val label: String):
    case AirMass   extends ElevationRangeType("Air Mass")
    case HourAngle extends ElevationRangeType("Hour Angle")

  private object ElevationRangeType:
    given Enumerated[ElevationRangeType] = Enumerated.from(AirMass, HourAngle).withTag(_.label)
    given Display[ElevationRangeType]    = Display.byShortName(_.label)

  import ElevationRangeType.*

  private case class ElevationRangeOptions(
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

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateBy((props, _) =>
        ElevationRangeOptions.fromElevationRange(props.constraintSet.elevationRange)
      )
      .useEffectWithDepsBy((props, _, _) => props.constraintSet.elevationRange)(
        (_, _, elevationRangeOptions) =>
          elevationRange => elevationRangeOptions.modState(_.toElevationRange(elevationRange))
      )
      .render { (props, ctx, elevationRangeOptions) =>
        import ctx.given

        val undoViewSet = UndoView(props.obsIds, props.undoCtx)

        val erView =
          undoViewSet(ConstraintSet.elevationRange, UpdateConstraintSet.elevationRange)

        def selectEnum[A: Enumerated: Display](
          label:     NonEmptyString,
          helpId:    Help.Id,
          lens:      Lens[ConstraintSet, A],
          remoteSet: A => ConstraintSetInput => ConstraintSetInput
        ) = {
          val id = NonEmptyString.unsafeFrom(label.value.toLowerCase().replaceAll(" ", "-"))
          React.Fragment(
            FormLabel(htmlFor = id)(label, HelpIcon(helpId)),
            EnumDropdownView(id = id,
                             value = undoViewSet(lens, remoteSet),
                             disabled = props.readonly
            )
          )
        }

        val erTypeView: View[ElevationRangeType] =
          View[ElevationRangeType](
            elevationRangeOptions.value.rangeType,
            (mod, cb) =>
              val previous = elevationRangeOptions.value.rangeType
              erView
                .setCB(
                  mod(elevationRangeOptions.value.rangeType) match
                    case AirMass   => elevationRangeOptions.value.airMass
                    case HourAngle => elevationRangeOptions.value.hourAngle,
                  _ match
                    case ElevationRange.AirMass(_, _)   => cb(previous, AirMass)
                    case ElevationRange.HourAngle(_, _) => cb(previous, HourAngle)
                )
          )

        val airMassView: View[ElevationRange.AirMass] =
          View[ElevationRange.AirMass](
            elevationRangeOptions.value.airMass,
            (mod, cb) =>
              erView
                .zoom(ElevationRange.airMass)
                .modCB(
                  mod,
                  (previous, current) => (previous, current).tupled.map((p, c) => cb(p, c)).orEmpty
                )
          )

        val hourAngleView: View[ElevationRange.HourAngle] =
          View[ElevationRange.HourAngle](
            elevationRangeOptions.value.hourAngle,
            (mod, cb) =>
              erView
                .zoom(ElevationRange.hourAngle)
                .modCB(
                  mod,
                  (previous, current) => (previous, current).tupled.map((p, c) => cb(p, c)).orEmpty
                )
          )

        React.Fragment(
          <.div(ExploreStyles.ConstraintsGrid)(
            selectEnum(
              "Image Quality".refined,
              "constraints/main/iq.md".refined,
              ConstraintSet.imageQuality,
              UpdateConstraintSet.imageQuality
            ),
            selectEnum(
              "Cloud Extinction".refined,
              "constraints/main/ce.md".refined,
              ConstraintSet.cloudExtinction,
              UpdateConstraintSet.cloudExtinction
            ),
            selectEnum(
              "Water Vapor".refined,
              "constraints/main/wv.md".refined,
              ConstraintSet.waterVapor,
              UpdateConstraintSet.waterVapor
            ),
            selectEnum(
              "Sky Background".refined,
              "constraints/main/sb.md".refined,
              ConstraintSet.skyBackground,
              UpdateConstraintSet.skyBackground
            ),
            FormLabel("ertype".refined)(
              "Elevation Range",
              HelpIcon("constraints/main/er.md".refined)
            ),
            <.div(
              ExploreStyles.ConstraintsElevationRangeGroup,
              EnumDropdownView(
                id = "ertype".refined,
                value = erTypeView,
                clazz = ExploreStyles.ElevationRangePicker,
                disabled = props.readonly
              ),
              React
                .Fragment(
                  <.label("Min"),
                  FormInputTextView(
                    id = "minam".refined,
                    value = airMassView.zoom(ElevationRange.AirMass.min),
                    validFormat = ModelValidators.airMassElevationRangeValidWedge.andThen(
                      ValidSplitEpiNec.lte(elevationRangeOptions.value.airMass.max,
                                           "Must be <= Max".refined
                      )
                    ),
                    changeAuditor = ChangeAuditor.accept.decimal(1.refined),
                    groupClass = ExploreStyles.ElevationRangeEntry,
                    disabled = props.readonly
                  ),
                  <.label("Max"),
                  FormInputTextView(
                    id = "maxam".refined,
                    value = airMassView.zoom(ElevationRange.AirMass.max),
                    validFormat = ModelValidators.airMassElevationRangeValidWedge.andThen(
                      ValidSplitEpiNec.gte(elevationRangeOptions.value.airMass.min,
                                           "Must be >= Min".refined
                      )
                    ),
                    changeAuditor = ChangeAuditor.accept.decimal(1.refined),
                    groupClass = ExploreStyles.ElevationRangeEntry,
                    disabled = props.readonly
                  )
                )
                .when(elevationRangeOptions.value.rangeType === AirMass),
              React
                .Fragment(
                  <.label("Min"),
                  FormInputTextView(
                    id = "minha".refined,
                    value = hourAngleView.zoom(ElevationRange.HourAngle.minHours),
                    validFormat = ModelValidators.hourAngleElevationRangeValidWedge.andThen(
                      ValidSplitEpiNec.lte(
                        elevationRangeOptions.value.hourAngle.maxHours,
                        "Must be <= Max".refined
                      )
                    ),
                    changeAuditor = ChangeAuditor.accept.decimal(1.refined),
                    groupClass = ExploreStyles.ElevationRangeEntry,
                    disabled = props.readonly
                  ),
                  <.label("Max"),
                  FormInputTextView(
                    id = "maxha".refined,
                    value = hourAngleView.zoom(ElevationRange.HourAngle.maxHours),
                    validFormat = ModelValidators.hourAngleElevationRangeValidWedge.andThen(
                      ValidSplitEpiNec.gte(
                        elevationRangeOptions.value.hourAngle.minHours,
                        "Must be >= Min".refined
                      )
                    ),
                    changeAuditor = ChangeAuditor.accept.decimal(1.refined),
                    groupClass = ExploreStyles.ElevationRangeEntry,
                    disabled = props.readonly
                  ),
                  <.label("Hours")
                )
                .when(elevationRangeOptions.value.rangeType === HourAngle)
            )
          )
        )
      }
