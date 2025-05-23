// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.ConstraintsQueries.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Help
import explore.model.ObsIdSet
import explore.model.formats.formatPercentile
import explore.undo.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.IntCentiPercent
import lucuma.core.model.validation.ModelValidators
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.validation.*
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.CentralWavelength
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.EnumDropdownView
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.given
import monocle.Lens

case class ConstraintsPanel(
  obsIds:                  ObsIdSet,
  obsIQLikelihood:         Option[IntCentiPercent],
  obsConditionsLikelihood: Option[IntCentiPercent],
  centralWavelength:       Option[CentralWavelength],
  undoCtx:                 UndoSetter[ConstraintSet],
  readonly:                Boolean
) extends ReactFnProps(ConstraintsPanel.component):
  val constraintSet: ConstraintSet = undoCtx.get

object ConstraintsPanel:
  private type Props = ConstraintsPanel

  private enum ElevationRangeType(val label: String, val tag: String) derives Enumerated:
    case ByAirMass   extends ElevationRangeType("Air Mass", "by_air_mass")
    case ByHourAngle extends ElevationRangeType("Hour Angle", "by_hour_angle")

  private object ElevationRangeType:
    given Display[ElevationRangeType] = Display.byShortName(_.label)

  import ElevationRangeType.*

  private case class ElevationRangeOptions(
    rangeType: ElevationRangeType,
    airMass:   ElevationRange.ByAirMass,
    hourAngle: ElevationRange.ByHourAngle
  ) {
    def toElevationRange(er: ElevationRange): ElevationRangeOptions =
      er match {
        case am @ ElevationRange.ByAirMass(_, _)   =>
          copy(rangeType = ElevationRangeType.ByAirMass, airMass = am)
        case ha @ ElevationRange.ByHourAngle(_, _) =>
          copy(rangeType = ElevationRangeType.ByHourAngle, hourAngle = ha)
      }
  }

  private object ElevationRangeOptions {
    def fromElevationRange(er: ElevationRange): ElevationRangeOptions =
      ElevationRangeOptions(
        ElevationRangeType.ByAirMass,
        ElevationRange.ByAirMass.Default,
        ElevationRange.ByHourAngle.Default
      ).toElevationRange(er)
  }

  private val component = ScalaFnComponent[Props]: props =>
    for {
      ctx                   <- useContext(AppContext.ctx)
      elevationRangeOptions <-
        useState(ElevationRangeOptions.fromElevationRange(props.constraintSet.elevationRange))
      _                     <- useEffectWithDeps(props.constraintSet.elevationRange)(elevationRange =>
                                 elevationRangeOptions.modState(_.toElevationRange(elevationRange))
                               )
    } yield {
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
          EnumDropdownView(id = id, value = undoViewSet(lens, remoteSet), disabled = props.readonly)
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
                  case ByAirMass   => elevationRangeOptions.value.airMass
                  case ByHourAngle => elevationRangeOptions.value.hourAngle,
                _ match
                  case ElevationRange.ByAirMass(_, _)   => cb(previous, ByAirMass)
                  case ElevationRange.ByHourAngle(_, _) => cb(previous, ByHourAngle)
              )
        )

      val airMassView: View[ElevationRange.ByAirMass] =
        View[ElevationRange.ByAirMass](
          elevationRangeOptions.value.airMass,
          (mod, cb) =>
            erView
              .zoom(ElevationRange.airMass)
              .modCB(
                mod,
                (previous, current) => (previous, current).tupled.map((p, c) => cb(p, c)).orEmpty
              )
        )

      val hourAngleView: View[ElevationRange.ByHourAngle] =
        View[ElevationRange.ByHourAngle](
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
          <.div(
            ExploreStyles.ConstraintsLikelihood,
            props.obsIQLikelihood.map(formatPercentile).getOrElse("(-)")
          ),
          selectEnum(
            "Cloud Extinction".refined,
            "constraints/main/ce.md".refined,
            ConstraintSet.cloudExtinction,
            UpdateConstraintSet.cloudExtinction
          ),
          <.div(ExploreStyles.ConstraintsLikelihood,
                formatPercentile(props.constraintSet.cloudExtinction.toCloudExtinction.percentile)
          ),
          selectEnum(
            "Water Vapor".refined,
            "constraints/main/wv.md".refined,
            ConstraintSet.waterVapor,
            UpdateConstraintSet.waterVapor
          ),
          <.div(ExploreStyles.ConstraintsLikelihood,
                formatPercentile(props.constraintSet.waterVapor.percentile)
          ),
          selectEnum(
            "Sky Background".refined,
            "constraints/main/sb.md".refined,
            ConstraintSet.skyBackground,
            UpdateConstraintSet.skyBackground
          ),
          <.div(ExploreStyles.ConstraintsLikelihood,
                formatPercentile(props.constraintSet.skyBackground.percentile)
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
                  value = airMassView.zoom(ElevationRange.ByAirMass.min),
                  validFormat = ModelValidators.AirMassElevationRangeValidWedge.andThen(
                    ValidSplitEpiNec.lte(
                      elevationRangeOptions.value.airMass.max,
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
                  value = airMassView.zoom(ElevationRange.ByAirMass.max),
                  validFormat = ModelValidators.AirMassElevationRangeValidWedge.andThen(
                    ValidSplitEpiNec.gte(elevationRangeOptions.value.airMass.min,
                                         "Must be >= Min".refined
                    )
                  ),
                  changeAuditor = ChangeAuditor.accept.decimal(1.refined),
                  groupClass = ExploreStyles.ElevationRangeEntry,
                  disabled = props.readonly
                )
              )
              .when(elevationRangeOptions.value.rangeType === ByAirMass),
            React
              .Fragment(
                <.label("Min"),
                FormInputTextView(
                  id = "minha".refined,
                  value = hourAngleView.zoom(ElevationRange.ByHourAngle.minHours),
                  validFormat = ModelValidators.HourAngleElevationRangeValidWedge.andThen(
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
                  value = hourAngleView.zoom(ElevationRange.ByHourAngle.maxHours),
                  validFormat = ModelValidators.HourAngleElevationRangeValidWedge.andThen(
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
              .when(elevationRangeOptions.value.rangeType === ByHourAngle)
          ),
          <.label(
            LucumaPrimeStyles.FormFieldLabel |+| ExploreStyles.ConstraintsSetLikelihood,
            "Likelihood of selected conditions",
            <.span(
              ExploreStyles.ConstraintsLikelihood,
              props.obsConditionsLikelihood.map(formatPercentile).getOrElse("(-)")
            )
          )
        )
      )
    }
