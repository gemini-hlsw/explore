// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import coulomb.ops.algebra.spire.all.given
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.auto.*
import explore.Icons
import explore.common.ScienceQueries.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model
import explore.model.AppContext
import explore.model.CoordinatesAtVizTime
import explore.model.ImagingConfigurationOptions
import explore.model.display.given
import explore.model.itc.ItcTarget
import explore.modes.SpectroscopyModesMatrix
import explore.undo.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ScienceMode
import lucuma.core.math.Coordinates
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.SiderealTracking
import lucuma.core.model.User
import lucuma.refined.*
import lucuma.ui.primereact.FormEnumDropdownView
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.schemas.odb.ObsQueries.*
import react.common.ReactFnProps
import react.primereact.Button

import scalajs.js.JSConverters.*
import lucuma.core.model.Program

case class BasicConfigurationPanel(
  userId:          Option[User.Id],
  programId:       Program.Id,
  obsId:           Observation.Id,
  requirementsCtx: UndoSetter[ScienceRequirementsData],
  scienceModeOpt:  View[Option[model.ScienceModeInitial]],
  constraints:     ConstraintSet,
  itcTargets:      List[ItcTarget],
  baseCoordinates: Option[CoordinatesAtVizTime],
  createConfig:    Callback,
  confMatrix:      SpectroscopyModesMatrix
) extends ReactFnProps(BasicConfigurationPanel.component)

private object BasicConfigurationPanel:
  private type Props = BasicConfigurationPanel

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView[ScienceMode](ScienceMode.Spectroscopy)
      .useStateView[ImagingConfigurationOptions](ImagingConfigurationOptions.Default)
      .render { (props, ctx, mode, imaging) =>
        import ctx.given

        val requirementsViewSet: ScienceRequirementsUndoView =
          ScienceRequirementsUndoView(props.programId, props.obsId, props.requirementsCtx)

        val isSpectroscopy: Boolean = mode.get === ScienceMode.Spectroscopy

        val spectroscopy: View[ScienceRequirementsData.Spectroscopy] =
          requirementsViewSet(
            ScienceRequirementsData.spectroscopy,
            UpdateScienceRequirements.spectroscopyRequirements
          )

        <.div(ExploreStyles.BasicConfigurationGrid)(
          <.div(
            ExploreStyles.BasicConfigurationForm,
            <.label("Mode", HelpIcon("configuration/mode.md".refined)),
            FormEnumDropdownView(id = "configuration-mode".refined, value = mode),
            SpectroscopyConfigurationPanel(spectroscopy)
              .when(isSpectroscopy),
            ImagingConfigurationPanel(imaging)
              .unless(isSpectroscopy)
          ),
          SpectroscopyModesTable(
            props.userId,
            props.scienceModeOpt,
            spectroscopy.get,
            props.constraints,
            if (props.itcTargets.isEmpty) none else props.itcTargets.some,
            props.baseCoordinates,
            props.confMatrix
          ).when(isSpectroscopy),
          <.div(ExploreStyles.BasicConfigurationButtons)(
            Button(
              "Accept Configuration",
              icon = Icons.Gears,
              // TODO: also need to disable if there is insufficient information for ITC
              disabled = props.scienceModeOpt.get.isEmpty,
              severity = Button.Severity.Secondary,
              onClick = props.createConfig
            ).compact.small
          )
        )
      }
