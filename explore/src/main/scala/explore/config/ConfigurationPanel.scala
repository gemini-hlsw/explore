// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import clue.data.Input
import clue.data.syntax._
import crystal.react._
import crystal.react.hooks._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.common.Aligner
import explore.common.ObsQueries._
import explore.common.ScienceQueries._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model
import explore.model.ITCTarget
import explore.model.ObsConfiguration
import explore.model.reusability._
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.reusability._
import monocle.std.option.some
import queries.common.ObsQueriesGQL
import react.common._

final case class ConfigurationPanel(
  obsId:            Observation.Id,
  obsConf:          ReuseView[ObsConfiguration],
  scienceData:      Reuse[UndoContext[ScienceData]],
  constraints:      ConstraintSet,
  itcTargets:       List[ITCTarget],
  renderInTitle:    Tile.RenderInTitle
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ConfigurationPanel](ConfigurationPanel.component)

object ConfigurationPanel {
  type Props = ConfigurationPanel

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewWithReuse(false) // showAdvanced
      .renderWithReuse { (props, showAdvanced) =>
        implicit val ctx: AppContextIO = props.ctx

        implicit val client = ctx.clients.odb // This shouldn't be necessary, but it seems to be

        val requirementsCtx: Reuse[UndoSetter[ScienceRequirementsData]] =
          props.scienceData.map(_.zoom(ScienceData.requirements))

        val modeAligner: Reuse[Aligner[Option[model.ScienceMode], Input[ScienceModeInput]]] =
          props.scienceData
            .map(ctx =>
              Aligner(
                ctx,
                EditObservationInput(props.obsId),
                // TODO Change to specific bulk mutation for science mode
                (ObsQueriesGQL.UpdateObservationMutation.execute[IO] _).andThen(_.void)
              ).zoom(
                ScienceData.mode,
                EditObservationInput.scienceMode.modify
              )
            )

        val optModeView: ReuseView[Option[model.ScienceMode]] =
          modeAligner.map(_.view(_.map(_.toInput).orUnassign))

        val modeViewOpt: ReuseViewOpt[model.ScienceMode] =
          optModeView.zoom(some[model.ScienceMode])

        val showBasicCB: Reuse[Callback] = showAdvanced.map(_.set(false))

        val showAdvancedCB: Reuse[Callback] = showAdvanced.map(_.set(true))

        React.Fragment(
          props.renderInTitle(
            <.span(ExploreStyles.TitleUndoButtons)(UndoButtons(props.scienceData))
          ),
          if (!showAdvanced.get)
            BasicConfigurationPanel(
              props.obsId,
              props.obsConf,
              requirementsCtx,
              optModeView,
              props.constraints,
              props.itcTargets,
              showAdvancedCB
            )
          else
            React.Fragment(
              // Gmos North Long Slit
              modeViewOpt
                .zoom(model.ScienceMode.gmosNorthLongSlit)
                .zoom(model.ScienceMode.GmosNorthLongSlit.advanced)
                .mapValue(view =>
                  AdvancedConfigurationPanel
                    .GmosNorthLongSlit(props.obsId, view, showBasicCB)
                ),
              // Gmos South Long Slit
              modeViewOpt
                .zoom(model.ScienceMode.gmosSouthLongSlit)
                .zoom(model.ScienceMode.GmosSouthLongSlit.advanced)
                .mapValue(view =>
                  AdvancedConfigurationPanel
                    .GmosSouthLongSlit(props.obsId, view, showBasicCB)
                )
            )
        )
      }

}
