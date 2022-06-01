// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import clue.data.Input
import clue.data.syntax._
import crystal.react._
import crystal.react.hooks._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
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
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB.Types._
import monocle.std.option.some
import queries.common.ObsQueriesGQL
import react.common._

final case class ConfigurationPanel(
  obsId:            Observation.Id,
  title:            String,
  subtitle:         Option[NonEmptyString],
  obsConf:          View[ObsConfiguration],
  scienceData:      UndoContext[ScienceData],
  constraints:      ConstraintSet,
  itcTargets:       List[ITCTarget],
  renderInTitle:    Tile.RenderInTitle
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ConfigurationPanel](ConfigurationPanel.component)

object ConfigurationPanel {
  type Props = ConfigurationPanel

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateView(false) // showAdvanced
      .render { (props, showAdvanced) =>
        implicit val ctx: AppContextIO = props.ctx

        implicit val client = ctx.clients.odb // This shouldn't be necessary, but it seems to be

        val requirementsCtx: UndoSetter[ScienceRequirementsData] =
          props.scienceData.zoom(ScienceData.requirements)

        val modeAligner: Aligner[Option[model.ScienceMode], Input[ScienceModeInput]] =
          Aligner(
            props.scienceData,
            EditObservationInput(
              select = ObservationSelectInput(observationIds = List(props.obsId).assign),
              patch = ObservationPropertiesInput()
            ),
            (ObsQueriesGQL.EditObservationMutation.execute[IO] _).andThen(_.void)
          ).zoom(
            ScienceData.mode,
            EditObservationInput.patch.andThen(ObservationPropertiesInput.scienceMode).modify
          )

        val optModeView: View[Option[model.ScienceMode]] =
          modeAligner.view(_.map(_.toInput).orUnassign)

        val modeViewOpt: ViewOpt[model.ScienceMode] =
          optModeView.zoom(some[model.ScienceMode])

        val showBasicCB: Callback = showAdvanced.set(false)

        val showAdvancedCB: Option[Callback] =
          optModeView.get.map(_ => showAdvanced.set(true))

        React.Fragment(
          props.renderInTitle(
            <.div(ExploreStyles.TitleUndoButtons)(UndoButtons(props.scienceData))
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
                .mapValue(view =>
                  AdvancedConfigurationPanel
                    .GmosNorthLongSlit(
                      props.obsId,
                      props.title,
                      props.subtitle,
                      view.zoom(model.ScienceMode.GmosNorthLongSlit.advanced),
                      view.zoom(model.ScienceMode.GmosNorthLongSlit.basic).get,
                      showBasicCB
                    )
                ),
              // Gmos South Long Slit
              modeViewOpt
                .zoom(model.ScienceMode.gmosSouthLongSlit)
                .mapValue(view =>
                  AdvancedConfigurationPanel
                    .GmosSouthLongSlit(
                      props.obsId,
                      props.title,
                      props.subtitle,
                      view.zoom(model.ScienceMode.GmosSouthLongSlit.advanced),
                      view.zoom(model.ScienceMode.GmosSouthLongSlit.basic).get,
                      showBasicCB
                    )
                )
            )
        )
      }

}
