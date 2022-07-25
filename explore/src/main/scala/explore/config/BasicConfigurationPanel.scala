// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all._
import crystal.react._
import crystal.react.hooks._
import eu.timepit.refined.auto._
import org.http4s.syntax.all._
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
import lucuma.core.optics.syntax.lens._
import lucuma.ui.forms.EnumViewSelect
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.elements.button.Button
import react.semanticui.shorthand._
import react.semanticui.sizes._

import explore.events.EventPicklers._
import scalajs.js.JSConverters._
import explore.modes.SpectroscopyModesMatrix
import explore.events.SpectroscopyMatrixRequest
import explore.model.boopickle._
import explore.events.SpectroscopyMatrixResults
import cats.effect.IO
import explore.events.WorkerMessage

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
      // Listen on web worker for messages with catalog candidates
      .useStreamOnMountBy((props, _, _) =>
        props.ctx.worker.stream
          .flatMap { r =>
            decodeFromTransferable[WorkerMessage](r) match {
              case Some(SpectroscopyMatrixResults(r)) =>
                fs2.Stream.emit[IO, SpectroscopyModesMatrix](r)
              case _                                  => fs2.Stream.raiseError[IO](new RuntimeException("Unknown worker message"))
            }
          }
      )
      .useEffectOnMountBy((p, _, _, _) =>
        p.ctx.worker
          .postWorkerMessage(SpectroscopyMatrixRequest(uri"/instrument_spectroscopy_matrix.csv"))
      )
      .render { (props, mode, imaging, matrix) =>
        implicit val ctx: AppContextIO = props.ctx
        println(matrix)

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
            <.label("Mode", HelpIcon("configuration/mode.md")),
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
            // ctx.staticData.spectroscopyMatrix
            SpectroscopyModesMatrix.empty
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
