// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import cats.syntax.all.*
import coulomb.ops.algebra.spire.all.given
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.implicits.*
import eu.timepit.refined.auto.*
import explore.Icons
import explore.common.ScienceQueries.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.BasicConfigAndItc
import explore.model.CoordinatesAtVizTime
import explore.model.ImagingConfigurationOptions
import explore.model.display.given
import explore.model.itc.ItcTarget
import explore.modes.SpectroscopyModesMatrix
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ScienceMode
import lucuma.core.math.Coordinates
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.refined.*
import lucuma.ui.primereact.FormEnumDropdownView
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.schemas.odb.ObsQueries.*
import react.common.ReactFnProps
import react.primereact.Button
import react.primereact.Message

import scalajs.js.JSConverters.*

case class BasicConfigurationPanel(
  userId:          Option[User.Id],
  programId:       Program.Id,
  obsId:           Observation.Id,
  requirementsCtx: UndoSetter[ScienceRequirementsData],
  selectedConfig:  View[Option[BasicConfigAndItc]],
  constraints:     ConstraintSet,
  itcTargets:      List[ItcTarget],
  baseCoordinates: Option[CoordinatesAtVizTime],
  createConfig:    IO[Unit],
  confMatrix:      SpectroscopyModesMatrix
) extends ReactFnProps(BasicConfigurationPanel.component)

private object BasicConfigurationPanel:
  private type Props = BasicConfigurationPanel

  private object Creating extends NewType[Boolean]
  private type Creating = Creating.Type

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView[ScienceMode](ScienceMode.Spectroscopy)
      .useStateView[ImagingConfigurationOptions](ImagingConfigurationOptions.Default)
      .useStateView(Creating(false))
      .render { (props, ctx, mode, imaging, creating) =>
        import ctx.given

        val requirementsViewSet: ScienceRequirementsUndoView =
          ScienceRequirementsUndoView(props.programId, props.obsId, props.requirementsCtx)

        val isSpectroscopy: Boolean = mode.get === ScienceMode.Spectroscopy

        val spectroscopy: View[ScienceRequirementsData.Spectroscopy] =
          requirementsViewSet(
            ScienceRequirementsData.spectroscopy,
            UpdateScienceRequirements.spectroscopyRequirements
          )

        val isMissingItc =
          props.itcTargets.isEmpty || spectroscopy.get.wavelength.isEmpty || spectroscopy.get.signalToNoise.isEmpty

        val creationWarning: Option[String] =
          if (isMissingItc) "To create a configuration, enter all information required for ITC".some
          else if (props.selectedConfig.get.isEmpty)
            "To create a configuration, select a table row".some
          else none

        val buttonIcon =
          if (creating.get.value) Icons.Spinner.withSpin(true)
          else Icons.Gears

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
            props.selectedConfig,
            spectroscopy.get,
            props.constraints,
            if (props.itcTargets.isEmpty) none else props.itcTargets.some,
            props.baseCoordinates,
            props.confMatrix
          ).when(isSpectroscopy),
          <.div(ExploreStyles.BasicConfigurationButtons)(
            creationWarning.map(m => Message(text = m)),
            Button(
              "Accept Configuration",
              icon = buttonIcon,
              disabled = creating.get.value || creationWarning.isDefined,
              severity = Button.Severity.Secondary,
              onClick = (creating.async.set(Creating(true)) >>
                props.createConfig.guarantee(creating.async.set(Creating(false)))).runAsync
            ).compact.small
          ).when(isSpectroscopy)
        )
      }
