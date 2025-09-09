// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.EitherNec
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Observation
import explore.model.ScienceRequirements
import explore.model.ScienceRequirements.Imaging
import explore.model.ScienceRequirements.Spectroscopy
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.model.itc.ItcTarget
import explore.model.itc.ItcTargetProblem
import explore.modes.ConfigSelection
import explore.modes.ScienceModes
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ScienceMode
import lucuma.core.model.ConstraintSet
import lucuma.core.model.CoordinatesAt
import lucuma.core.model.User
import lucuma.core.util.NewBoolean
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.primereact.Button
import lucuma.react.primereact.Tag
import lucuma.refined.*
import lucuma.ui.LucumaIcons
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given

case class BasicConfigurationPanel(
  userId:              Option[User.Id],
  obsId:               Observation.Id,
  requirementsView:    View[ScienceRequirements],
  selectedConfig:      View[ConfigSelection],
  constraints:         ConstraintSet,
  itcTargets:          EitherNec[ItcTargetProblem, NonEmptyList[ItcTarget]],
  baseCoordinates:     Option[CoordinatesAt],
  calibrationRole:     Option[CalibrationRole],
  createConfig:        IO[Unit],
  confMatrix:          ScienceModes,
  customSedTimestamps: List[Timestamp],
  readonly:            Boolean,
  units:               WavelengthUnits,
  targetView:          View[Option[ItcTarget]]
) extends ReactFnProps(BasicConfigurationPanel.component)

private object BasicConfigurationPanel:
  private type Props = BasicConfigurationPanel

  private object Creating extends NewBoolean

  private val component =
    ScalaFnComponent[Props]: props =>
      for
        ctx             <- useContext(AppContext.ctx)
        scienceModeType <- useStateView[ScienceMode](ScienceMode.Spectroscopy)
        _               <- useEffectWithDeps(props.requirementsView.get.scienceModeType): modeType =>
                             scienceModeType.set(modeType)
        creating        <- useStateView(Creating(false))
      yield
        import ctx.given

        val canAccept: Boolean = props.selectedConfig.get.canAccept

        val spectroscopyView: ViewOpt[Spectroscopy] = props.requirementsView
          .zoom(ScienceRequirements.spectroscopy)

        val imagingView: ViewOpt[Imaging] = props.requirementsView
          .zoom(ScienceRequirements.imaging)

        val exposureTimeView = props.requirementsView
          .zoom(ScienceRequirements.exposureTimeMode)

        // wavelength has to be handled special for spectroscopy because you can't select a row without a wavelength.
        val message: Option[String] =
          if (spectroscopyView.get.exists(_.wavelength.isEmpty))
            "Wavelength is required for creating a configuration.".some
          else if (
            props.selectedConfig.get.hasItcErrors || props.selectedConfig.get.isMissingSomeItc
          )
            "ITC issues must be fixed.".some
          else if (props.selectedConfig.get.hasPendingItc)
            "Waiting for ITC result...".some
          else if (props.selectedConfig.get.isEmpty)
            "To create a configuration, select a table row.".some
          else none

        def switchMode(scienceModeType: ScienceMode): Callback =
          val newScienceMode = scienceModeType match
            case ScienceMode.Spectroscopy => ScienceRequirements.Spectroscopy.Default.asLeft
            case ScienceMode.Imaging      => ScienceRequirements.Imaging.Default.asRight
          props.requirementsView
            .zoom(ScienceRequirements.scienceMode)
            .set(
              newScienceMode
            )

        val buttonIcon: FontAwesomeIcon =
          if (creating.get.value) Icons.Spinner.withSpin(true)
          else LucumaIcons.Gears

        <.div(ExploreStyles.BasicConfigurationGrid)(
          <.div(
            ExploreStyles.BasicConfigurationForm,
            FormEnumDropdownView(
              id = "configuration-mode".refined,
              label = React.Fragment("Mode", HelpIcon("configuration/mode.md".refined)),
              value = scienceModeType.withOnMod(switchMode),
              disabled = props.readonly
            ),
            spectroscopyView.mapValue: s =>
              SpectroscopyConfigurationPanel(
                props.selectedConfig.get.headOption.map(_.instrument),
                exposureTimeView,
                s,
                props.readonly,
                props.units,
                props.calibrationRole
              ),
            imagingView.mapValue: s =>
              ImagingConfigurationPanel(
                props.selectedConfig.get.headOption.map(_.instrument),
                exposureTimeView,
                s,
                props.readonly,
                props.units,
                props.calibrationRole
              )
          ),
          spectroscopyView
            .mapValue(s =>
              SpectroscopyModesTable(
                props.userId,
                props.selectedConfig,
                exposureTimeView.get,
                s.get,
                props.constraints,
                props.itcTargets,
                props.baseCoordinates,
                props.confMatrix.spectroscopy,
                props.customSedTimestamps,
                props.units
              )
            ),
          imagingView.mapValue(s =>
            ImagingModesTable(
              props.userId,
              props.selectedConfig,
              exposureTimeView.get,
              s.get,
              props.confMatrix.imaging,
              props.constraints,
              props.itcTargets,
              props.baseCoordinates,
              props.customSedTimestamps,
              props.units,
              props.targetView
            )
          ),
          <.div(ExploreStyles.BasicConfigurationButtons)(
            message.map(Tag(_, severity = Tag.Severity.Success)),
            Button(
              "Accept Configuration",
              icon = buttonIcon,
              disabled = creating.get.value || !canAccept,
              severity = Button.Severity.Primary,
              onClick = props.createConfig.switching(creating.async, Creating(_)).runAsync
            ).compact.small.when(canAccept)
          ).unless(props.readonly)
        )
