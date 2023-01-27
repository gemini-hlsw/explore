// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Eq
import cats.effect.IO
import cats.syntax.all.*
import clue.TransactionalClient
import clue.data.Assign
import clue.data.Input
import clue.data.syntax.*
import crystal.PotOption
import crystal.implicits.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.implicits.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.common.Aligner
import explore.common.ScienceConversions.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.events.*
import explore.model
import explore.model.AppContext
import explore.model.CoordinatesAtVizTime
import explore.model.ScienceModeInitial
import explore.model.WorkerClients.*
import explore.model.boopickle.Boopickle.*
import explore.model.boopickle.ItcPicklers.given
import explore.model.boopickle.*
import explore.model.enums.AgsState
import explore.model.itc.ItcTarget
import explore.model.reusability.given
import explore.modes.SpectroscopyModesMatrix
import explore.undo.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.User
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.http4s.syntax.all.*
import queries.common.ObsQueriesGQL
import queries.schemas.odb.ObsQueries.*
import react.common.ReactFnProps

case class ConfigurationPanel(
  userId:          Option[User.Id],
  programId:       Program.Id,
  obsId:           Observation.Id,
  title:           String,
  subtitle:        Option[NonEmptyString],
  scienceData:     UndoContext[ScienceData],
  constraints:     ConstraintSet,
  itcTargets:      List[ItcTarget],
  baseCoordinates: Option[CoordinatesAtVizTime],
  agsState:        View[AgsState],
  selectedPA:      Option[Angle],
  renderInTitle:   Tile.RenderInTitle
) extends ReactFnProps[ConfigurationPanel](ConfigurationPanel.component)

object ConfigurationPanel:
  private type Props = ConfigurationPanel

  // TODO: The following few methods could be moved to `clue` if they are appropiate. Before
  // doing so, I'd like to have the code reviewed and perhaps looked over by `Mr. Clue` so
  // he can point out a much easier path. :P
  // The particular problem they solve here is that ScienceModeInput can have either a
  // gmosNorthLongSlitInput or a gmosSouthLongSlitInput, but not both. And we can't know
  // until we edit.

  extension [A](input: Input[A])
    /**
     * If the Input is not `Assing[A]`, create a new Input with the parameter and `assign` it.
     */
    def orAssign(ifNotAssigned: => A): Input[A] = input match
      case Assign(_) => input
      case _         => ifNotAssigned.assign

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

  // TODO: We probably want a mutation that returns the configuration so that we can update locally
  private def createConfiguration(
    programId: Program.Id,
    obsId:     Observation.Id,
    mode:      Option[ScienceModeInitial]
  )(using TransactionalClient[IO, ObservationDB]) =
    mode.foldMap(m =>
      ObsQueriesGQL.UpdateObservationMutation
        .execute[IO](
          UpdateObservationsInput(
            programId = programId,
            WHERE = obsId.toWhereObservation.assign,
            SET = ObservationPropertiesInput(observingMode = m.toInput.assign)
          )
        )
        .void
    )

  private given Eq[SpectroscopyModesMatrix] = Eq.by(_.matrix.isEmpty)

  private given Reusability[SpectroscopyModesMatrix] = Reusability.byEq

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // TODO: We probably want to move this to ObsTabContents.
      .useStateView(none[ScienceModeInitial])
      .useEffectResultOnMountBy { (props, ctx, _) =>
        import ctx.given

        ItcClient[IO]
          .requestSingle(
            ItcMessage.SpectroscopyMatrixRequest(uri"/instrument_spectroscopy_matrix.csv")
          )
      }
      .render { (props, ctx, scienceModeInitial, matrix) =>
        import ctx.given

        val requirementsCtx: UndoSetter[ScienceRequirementsData] =
          props.scienceData.zoom(ScienceData.requirements)

        val modeAligner: Aligner[Option[model.ScienceMode], Input[ObservingModeInput]] =
          Aligner(
            props.scienceData,
            UpdateObservationsInput(
              props.programId,
              WHERE = props.obsId.toWhereObservation.assign,
              SET = ObservationPropertiesInput(observingMode = ObservingModeInput().assign)
            ),
            (ObsQueriesGQL.UpdateObservationMutation.execute[IO] _).andThen(_.void)
          ).zoom(
            ScienceData.mode,
            UpdateObservationsInput.SET.andThen(ObservationPropertiesInput.observingMode).modify
          )

        val optModeView: View[Option[model.ScienceMode]] =
          modeAligner.view(_.map(_.toInput).orUnassign)

        val deleteConfiguration = optModeView.set(none)

        val optModeAligner = modeAligner.toOption

        val posAngleView: View[PosAngleConstraint] =
          props.scienceData.undoableView(ScienceData.posAngle)

        val optNorthAligner = optModeAligner.flatMap {
          _.zoomOpt(
            model.ScienceMode.gmosNorthLongSlit,
            modOrAssignAndMap(GmosNorthLongSlitInput())(
              ObservingModeInput.gmosNorthLongSlit.modify
            )
          )
        }

        val optSouthAligner = optModeAligner.flatMap {
          _.zoomOpt(
            model.ScienceMode.gmosSouthLongSlit,
            modOrAssignAndMap(GmosSouthLongSlitInput())(ObservingModeInput.gmosSouthLongSlit.modify)
          )
        }

        val confMatrix = matrix.toOption.flatten.getOrElse(SpectroscopyModesMatrix.empty)

        React.Fragment(
          props.renderInTitle(
            <.div(ExploreStyles.TitleUndoButtons)(UndoButtons(props.scienceData))
          ),
          <.div(ExploreStyles.ConfigurationGrid)(
            PAConfigurationPanel(props.programId,
                                 props.obsId,
                                 posAngleView,
                                 props.selectedPA,
                                 props.agsState
            ),
            if (optModeView.get.isEmpty)
              BasicConfigurationPanel(
                props.userId,
                props.programId,
                props.obsId,
                requirementsCtx,
                scienceModeInitial,
                props.constraints,
                props.itcTargets,
                props.baseCoordinates,
                createConfiguration(props.programId, props.obsId, scienceModeInitial.get).runAsync,
                confMatrix
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
                      northAligner,
                      requirementsCtx.model.get.spectroscopy,
                      props.scienceData.model.zoom(ScienceData.potITC),
                      deleteConfiguration,
                      confMatrix
                    )
                ),
                // Gmos South Long Slit
                optSouthAligner.map(southAligner =>
                  AdvancedConfigurationPanel
                    .GmosSouthLongSlit(
                      props.obsId,
                      props.title,
                      props.subtitle,
                      southAligner,
                      requirementsCtx.model.get.spectroscopy,
                      props.scienceData.model.zoom(ScienceData.potITC),
                      deleteConfiguration,
                      confMatrix
                    )
                )
              )
          )
        )
      }
