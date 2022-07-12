// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import clue.data.Assign
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
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.SiderealTracking
import lucuma.schemas.ObservationDB.Types._
import queries.common.ObsQueriesGQL
import queries.schemas.implicits._
import react.common._

final case class ConfigurationPanel(
  obsId:            Observation.Id,
  title:            String,
  subtitle:         Option[NonEmptyString],
  scienceData:      UndoContext[ScienceData],
  constraints:      ConstraintSet,
  itcTargets:       List[ITCTarget],
  baseTracking:     Option[SiderealTracking],
  renderInTitle:    Tile.RenderInTitle
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ConfigurationPanel](ConfigurationPanel.component)

object ConfigurationPanel {
  type Props = ConfigurationPanel

  // TODO: The following few methods could be moved to `clue` if they are appropiate. Before
  // doing so, I'd like to have the code reviewed and perhaps looked over by `Mr. Clue` so
  // he can point out a much easier path. :P
  // The particular problem they solve here is that ScienceModeInput can have either a
  // gmosNorthLongSlitInput or a gmosSouthLongSlitInput, but not both. And we can't know
  // until we edit.

  implicit class InputOps[A](input: Input[A]) {

    /**
     * If the Input is not `Assing[A]`, create a new Input with the parameter and `assign` it.
     */
    def orAssign(ifNotAssigned: => A): Input[A] = input match {
      case Assign(_) => input
      case _         => ifNotAssigned.assign
    }
  }

  /**
   * Handles the case where `A.Input[B]` not have an assigned value, but it needs to be created for
   * the `mod` function to work on.
   */
  private def mapModOrAssign[A, B](
    ifNotAssigned: => B
  )(
    mod:           (Input[B] => Input[B]) => A => A
  ): (Input[B] => Input[B]) => Input[A] => Input[A] =
    f =>
      _.map(mod { i =>
        val iassign = i.orAssign(ifNotAssigned)
        f(iassign)
      })

  /**
   * Handles the case where `A.Input[B]` not have an assigned value, but it needs to be created for
   * the `mod` function to work on.
   */
  private def modOrAssignAndMap[A, B](
    ifNotAssigned: => B
  )(mod:           (Input[B] => Input[B]) => A => A): (B => B) => Input[A] => Input[A] =
    f =>
      _.map(mod { ib =>
        val bAssign = ib.orAssign(ifNotAssigned)
        bAssign.map(f)
      })

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
            UpdateObservationsInput(
              WHERE = props.obsId.toWhereObservation.assign,
              SET = ObservationPropertiesInput(scienceMode = ScienceModeInput().assign)
            ),
            (ObsQueriesGQL.UpdateObservationMutation.execute[IO] _).andThen(_.void)
          ).zoom(
            ScienceData.mode,
            UpdateObservationsInput.SET.andThen(ObservationPropertiesInput.scienceMode).modify
          )

        val optModeView: View[Option[model.ScienceMode]] =
          modeAligner.view(_.map(_.toInput).orUnassign)

        val optModeAligner = modeAligner.toOption

        val showBasicCB: Callback = showAdvanced.set(false)

        val showAdvancedCB: Option[Callback] =
          optModeAligner.map(_ => showAdvanced.set(true))

        val posAngleView: View[Option[PosAngleConstraint]] =
          props.scienceData.undoableView(ScienceData.posAngle)

        val optNorthAligner = optModeAligner.flatMap {
          _.zoomOpt(
            model.ScienceMode.gmosNorthLongSlit,
            mapModOrAssign(GmosNorthLongSlitInput())(ScienceModeInput.gmosNorthLongSlit.modify)
          )
        }

        val optSouthAligner = optModeAligner.flatMap {
          _.zoomOpt(
            model.ScienceMode.gmosSouthLongSlit,
            mapModOrAssign(GmosSouthLongSlitInput())(ScienceModeInput.gmosSouthLongSlit.modify)
          )
        }

        React.Fragment(
          props.renderInTitle(
            <.div(ExploreStyles.TitleUndoButtons)(UndoButtons(props.scienceData))
          ),
          if (!showAdvanced.get)
            <.div(ExploreStyles.ConfigurationGrid)(
              ObsConfigurationPanel(props.obsId, posAngleView),
              BasicConfigurationPanel(
                props.obsId,
                requirementsCtx,
                optModeView,
                props.constraints,
                props.itcTargets,
                props.baseTracking,
                showAdvancedCB
              )
            )
          else
            React.Fragment(
              // Gmos North Long Slit
              optNorthAligner.map(northAligner =>
                AdvancedConfigurationPanel
                  .GmosNorthLongSlit(
                    props.obsId,
                    props.title,
                    props.subtitle,
                    northAligner.zoom(
                      model.ScienceMode.GmosNorthLongSlit.advanced,
                      modOrAssignAndMap(GmosNorthLongSlitAdvancedConfigInput())(
                        GmosNorthLongSlitInput.advanced.modify
                      )
                    ),
                    northAligner.get.basic,
                    props.scienceData.model.zoom(ScienceData.potITC),
                    showBasicCB
                  )
              ),
              // Gmos South Long Slit
              optSouthAligner.map(southAligner =>
                AdvancedConfigurationPanel
                  .GmosSouthLongSlit(
                    props.obsId,
                    props.title,
                    props.subtitle,
                    southAligner.zoom(
                      model.ScienceMode.GmosSouthLongSlit.advanced,
                      modOrAssignAndMap(GmosSouthLongSlitAdvancedConfigInput())(
                        GmosSouthLongSlitInput.advanced.modify
                      )
                    ),
                    southAligner.get.basic,
                    props.scienceData.model.zoom(ScienceData.potITC),
                    showBasicCB
                  )
              )
            )
        )
      }

}
