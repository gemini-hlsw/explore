// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import _root_.react.common.ReactFnProps
import cats.Eq
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.Assign
import clue.data.Input
import clue.data.syntax.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.DefaultErrorPolicy
import explore.*
import explore.common.Aligner
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.events.*
import explore.model.AppContext
import explore.model.BasicConfigAndItc
import explore.model.ObsConfiguration
import explore.model.ScienceRequirements
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
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.User
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Iso
import org.http4s.syntax.all.*
import queries.common.ObsQueriesGQL
import queries.schemas.odb.ObsQueries.*

case class ConfigurationPanel(
  userId:          Option[User.Id],
  programId:       Program.Id,
  obsId:           Observation.Id,
  title:           String,
  subtitle:        Option[NonEmptyString],
  requirements:    UndoSetter[ScienceRequirements],
  mode:            UndoSetter[Option[ObservingMode]],
  posAngle:        View[PosAngleConstraint],
  obsConf:         ObsConfiguration,
  itcTargets:      List[ItcTarget],
  baseCoordinates: Option[CoordinatesAtVizTime],
  selectedConfig:  View[Option[BasicConfigAndItc]]
) extends ReactFnProps[ConfigurationPanel](ConfigurationPanel.component)

object ConfigurationPanel:
  private type Props = ConfigurationPanel

  // TODO: The following few methods could be moved to `clue` if they are appropiate. Before
  // doing so, I'd like to have the code reviewed and perhaps looked over by `Mr. Clue` so
  // he can point out a much easier path. :P
  // The particular problem they solve here is that ObservingModeInput can have either a
  // gmosNorthLongSlitInput or a gmosSouthLongSlitInput, but not both. And we can't know
  // until we edit.

  extension [A](input: Input[A])
    /**
     * If the Input is not `Assign[A]`, create a new Input with the parameter and `assign` it.
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
  )(mod: (Input[B] => Input[B]) => A => A): (B => B) => Input[A] => Input[A] =
    f =>
      _.map(mod { ib =>
        val bAssign = ib.orAssign(ifNotAssigned)
        bAssign.map(f)
      })

  // TODO: We probably want a mutation that returns the configuration so that we can update locally
  private def createConfiguration(
    programId:     Program.Id,
    obsId:         Observation.Id,
    config:        Option[BasicConfiguration],
    observingMode: View[Option[ObservingMode]]
  )(using FetchClient[IO, ObservationDB]): IO[Unit] =
    config.foldMap(c =>
      ObsQueriesGQL
        .CreateConfigurationMutation[IO]
        .execute(
          UpdateObservationsInput(
            programId = programId,
            WHERE = obsId.toWhereObservation.assign,
            SET = ObservationPropertiesInput(observingMode = c.toInput.assign)
          )
        )
        .flatMap(data =>
          observingMode
            .set(data.updateObservations.observations.headOption.flatMap(_.observingMode))
            .toAsync
        )
    )

  private given Eq[SpectroscopyModesMatrix] = Eq.by(_.matrix.isEmpty)

  private given Reusability[SpectroscopyModesMatrix] = Reusability.byEq

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useEffectResultOnMountBy { (props, ctx) =>
        import ctx.given

        ItcClient[IO]
          .requestSingle(
            ItcMessage.SpectroscopyMatrixRequest(uri"/instrument_spectroscopy_matrix.csv")
          )
      }
      .render { (props, ctx, matrix) =>
        import ctx.given

        val modeAligner: Aligner[Option[ObservingMode], Input[ObservingModeInput]] =
          Aligner(
            props.mode,
            UpdateObservationsInput(
              props.programId,
              WHERE = props.obsId.toWhereObservation.assign,
              SET = ObservationPropertiesInput(observingMode = ObservingModeInput().assign)
            ),
            (ObsQueriesGQL.UpdateObservationMutation[IO].execute(_)).andThen(_.void)
          ).zoom( // Can we avoid the zoom and make an Aligner constructor that takes an input value?
            Iso.id,
            UpdateObservationsInput.SET.andThen(ObservationPropertiesInput.observingMode).modify
          )

        val optModeView: View[Option[ObservingMode]] =
          modeAligner.view(_.map(_.toInput).orUnassign)

        val deleteConfiguration = optModeView.set(none)

        val optModeAligner = modeAligner.toOption

        val optNorthAligner = optModeAligner.flatMap {
          _.zoomOpt(
            ObservingMode.gmosNorthLongSlit,
            modOrAssignAndMap(GmosNorthLongSlitInput())(
              ObservingModeInput.gmosNorthLongSlit.modify
            )
          )
        }

        val optSouthAligner = optModeAligner.flatMap {
          _.zoomOpt(
            ObservingMode.gmosSouthLongSlit,
            modOrAssignAndMap(GmosSouthLongSlitInput())(ObservingModeInput.gmosSouthLongSlit.modify)
          )
        }

        val confMatrix = matrix.toOption.flatten.getOrElse(SpectroscopyModesMatrix.empty)

        React.Fragment(
          <.div(ExploreStyles.ConfigurationGrid)(
            props.obsConf.agsState
              .map(agsState =>
                PAConfigurationPanel(
                  props.programId,
                  props.obsId,
                  props.posAngle,
                  props.obsConf.selectedPA,
                  props.obsConf.averagePA,
                  agsState
                )
              ),
            if (optModeView.get.isEmpty)
              props.obsConf.constraints
                .map(constraints =>
                  BasicConfigurationPanel(
                    props.userId,
                    props.programId,
                    props.obsId,
                    props.requirements,
                    props.selectedConfig,
                    constraints,
                    props.itcTargets,
                    props.baseCoordinates,
                    createConfiguration(
                      props.programId,
                      props.obsId,
                      props.selectedConfig.get.map(_.configuration),
                      optModeView
                    ),
                    confMatrix
                  )
                )
            else
              ScienceRequirements.spectroscopy
                .getOption(props.requirements.model.get)
                .map(spectroscopyRequirements =>
                  React.Fragment(
                    // Gmos North Long Slit
                    optNorthAligner.map(northAligner =>
                      AdvancedConfigurationPanel
                        .GmosNorthLongSlit(
                          props.programId,
                          props.obsId,
                          props.title,
                          props.subtitle,
                          northAligner,
                          spectroscopyRequirements,
                          deleteConfiguration,
                          confMatrix,
                          props.selectedConfig
                        )
                    ),
                    // Gmos South Long Slit
                    optSouthAligner.map(southAligner =>
                      AdvancedConfigurationPanel
                        .GmosSouthLongSlit(
                          props.programId,
                          props.obsId,
                          props.title,
                          props.subtitle,
                          southAligner,
                          spectroscopyRequirements,
                          deleteConfiguration,
                          confMatrix,
                          props.selectedConfig
                        )
                    )
                  )
                )
          )
        )
      }
