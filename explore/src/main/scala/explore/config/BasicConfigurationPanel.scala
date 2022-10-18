// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.schemas.odb.ObsQueries.*
import react.common.ReactFnProps
import react.semanticui.collections.form.Form
import react.semanticui.elements.button.Button
import react.semanticui.shorthand.*
import react.semanticui.sizes.*

import scalajs.js.JSConverters.*

case class BasicConfigurationPanel(
  userId:          Option[User.Id],
  obsId:           Observation.Id,
  requirementsCtx: UndoSetter[ScienceRequirementsData],
  scienceModeOpt:  View[Option[model.ScienceMode]],
  constraints:     ConstraintSet,
  itcTargets:      List[ItcTarget],
  baseCoordinates: Option[CoordinatesAtVizTime],
  onShowDetails:   Callback,
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
          ScienceRequirementsUndoView(props.obsId, props.requirementsCtx)

        val isSpectroscopy: Boolean = mode.get === ScienceMode.Spectroscopy

        val spectroscopy: View[ScienceRequirementsData.Spectroscopy] =
          requirementsViewSet(
            ScienceRequirementsData.spectroscopy,
            UpdateScienceRequirements.spectroscopyRequirements
          )

        <.div(ExploreStyles.BasicConfigurationGrid)(
          Form(size = Small)(
            ExploreStyles.Compact,
            ExploreStyles.ExploreForm,
            ExploreStyles.BasicConfigurationForm
          )(
            <.label("Mode", HelpIcon("configuration/mode.md".refined)),
            EnumViewSelect(id = "configuration-mode", value = mode),
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
            props.confMatrix,
            props.onShowDetails
          ).when(isSpectroscopy),
          <.div(ExploreStyles.BasicConfigurationButtons)(
            Button(
              size = Small,
              compact = true,
              content = "View Details",
              icon = Icons.Gears,
              disabled = props.scienceModeOpt.get.isEmpty,
              onClick = props.onShowDetails
            )
          )
        )
      }
