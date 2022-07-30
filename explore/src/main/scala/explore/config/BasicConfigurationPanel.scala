// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all._
import coulomb.ops.algebra.spire.all.given
import crystal.react._
import crystal.react.hooks._
import eu.timepit.refined.auto._
import explore.Icons
import explore.common.ObsQueries._
import explore.common.ScienceQueries._
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model
import explore.model.ITCTarget
import explore.model.ImagingConfigurationOptions
import explore.model.display._
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enums.ScienceMode
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.SiderealTracking
import lucuma.refined.*
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.semanticui.collections.form.Form
import react.semanticui.elements.button.Button
import react.semanticui.shorthand._
import react.semanticui.sizes._

import scalajs.js.JSConverters._

final case class BasicConfigurationPanel(
  obsId:            Observation.Id,
  requirementsCtx:  UndoSetter[ScienceRequirementsData],
  scienceModeOpt:   View[Option[model.ScienceMode]],
  constraints:      ConstraintSet,
  itcTargets:       List[ITCTarget],
  baseTracking:     Option[SiderealTracking],
  onShowAdvanced:   Option[Callback]
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[BasicConfigurationPanel](BasicConfigurationPanel.component)

object BasicConfigurationPanel {
  type Props = BasicConfigurationPanel

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateView[ScienceMode](ScienceMode.Spectroscopy)
      .useStateView[ImagingConfigurationOptions](ImagingConfigurationOptions.Default)
      .render { (props, mode, imaging) =>
        implicit val ctx: AppContextIO = props.ctx

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
            props.scienceModeOpt,
            spectroscopy.get,
            props.constraints,
            if (props.itcTargets.isEmpty) none else props.itcTargets.some,
            props.baseTracking,
            ctx.staticData.spectroscopyMatrix
          ).when(isSpectroscopy),
          <.div(ExploreStyles.BasicConfigurationButtons)(
            Button(
              size = Small,
              compact = true,
              content = "Advanced Configuration",
              icon = Icons.Gears,
              disabled = props.onShowAdvanced.isEmpty,
              onClick = props.onShowAdvanced.orUndefined
            )
          )
        )
      }

}
