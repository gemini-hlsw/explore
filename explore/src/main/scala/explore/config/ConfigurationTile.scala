// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.Assign
import clue.data.Input
import clue.data.syntax.*
import crystal.*
import crystal.react.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.DefaultErrorPolicy
import explore.common.Aligner
import explore.common.ScienceQueries.ScienceRequirementsUndoView
import explore.common.ScienceQueries.UpdateScienceRequirements
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.AsterismIds
import explore.model.InstrumentConfigAndItcResult
import explore.model.ObsConfiguration
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.ScienceRequirements
import explore.model.ScienceRequirements.Spectroscopy
import explore.model.TargetList
import explore.model.enums.WavelengthUnits
import explore.model.itc.ItcTarget
import explore.modes.InstrumentConfig
import explore.modes.SpectroscopyModesMatrix
import explore.undo.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.react.common.ReactFnProps
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.syntax.all.given
import monocle.Iso
import org.typelevel.log4cats.Logger
import queries.common.ObsQueriesGQL
import queries.schemas.itc.syntax.*

object ConfigurationTile:
  def apply(
    userId:                   Option[User.Id],
    programId:                Program.Id,
    obsId:                    Observation.Id,
    requirements:             UndoSetter[ScienceRequirements],
    mode:                     UndoSetter[Option[ObservingMode]],
    posAngleConstraint:       View[PosAngleConstraint],
    scienceTargetIds:         AsterismIds,
    baseCoordinates:          Option[CoordinatesAtVizTime],
    obsConf:                  ObsConfiguration,
    selectedConfig:           View[Option[InstrumentConfigAndItcResult]],
    revertedInstrumentConfig: Option[InstrumentConfig], // configuration selected if reverted
    modes:                    SpectroscopyModesMatrix,
    allTargets:               TargetList,
    sequenceChanged:          Callback,
    readonly:                 Boolean,
    units:                    WavelengthUnits
  )(using Logger[IO]) =
    Tile(
      ObsTabTileIds.ConfigurationId.id,
      "Configuration",
      bodyClass = ExploreStyles.ConfigurationTileBody
    )(_ =>
      Body(
        userId,
        programId,
        obsId,
        requirements,
        mode,
        posAngleConstraint,
        obsConf,
        scienceTargetIds.itcTargets(allTargets),
        baseCoordinates,
        selectedConfig,
        revertedInstrumentConfig,
        modes,
        sequenceChanged,
        readonly,
        units
      )
    )

  private case class Body(
    userId:                   Option[User.Id],
    programId:                Program.Id,
    obsId:                    Observation.Id,
    requirements:             UndoSetter[ScienceRequirements],
    mode:                     UndoSetter[Option[ObservingMode]],
    posAngle:                 View[PosAngleConstraint],
    obsConf:                  ObsConfiguration,
    itcTargets:               List[ItcTarget],
    baseCoordinates:          Option[CoordinatesAtVizTime],
    selectedConfig:           View[Option[InstrumentConfigAndItcResult]],
    revertedInstrumentConfig: Option[InstrumentConfig],
    modes:                    SpectroscopyModesMatrix,
    sequenceChanged:          Callback,
    readonly:                 Boolean,
    units:                    WavelengthUnits
  ) extends ReactFnProps[Body](Body.component)

  private object Body:
    private type Props = Body

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
     * Handles the case where `A.Input[B]` not have an assigned value, but it needs to be created
     * for the `mod` function to work on.
     */
    def mapModOrAssign[A, B](
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
     * Handles the case where `A.Input[B]` not have an assigned value, but it needs to be created
     * for the `mod` function to work on.
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
      obsId:         Observation.Id,
      config:        Option[BasicConfiguration],
      observingMode: View[Option[ObservingMode]]
    )(using FetchClient[IO, ObservationDB]): IO[Unit] =
      config.foldMap(c =>
        ObsQueriesGQL
          .CreateConfigurationMutation[IO]
          .execute(
            UpdateObservationsInput(
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

    private val component =
      ScalaFnComponent
        .withHooks[Props]
        .useContext(AppContext.ctx)
        .render { (props, ctx) =>
          import ctx.given

          val modeAligner: Aligner[Option[ObservingMode], Input[ObservingModeInput]] =
            Aligner(
              props.mode,
              UpdateObservationsInput(
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

          val revertConfiguration: Callback =
            optModeView.set(none) >>
              props.revertedInstrumentConfig
                .map: row => // Select the reverted config
                  props.selectedConfig.mod: c =>
                    InstrumentConfigAndItcResult(
                      row,
                      c.flatMap(_.itcResult.flatMap(_.toOption.map(_.asRight)))
                    ).some
                .orEmpty

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
              modOrAssignAndMap(GmosSouthLongSlitInput())(
                ObservingModeInput.gmosSouthLongSlit.modify
              )
            )
          }

          val requirementsViewSet: ScienceRequirementsUndoView =
            ScienceRequirementsUndoView(props.obsId, props.requirements)

          val requirementsView: View[ScienceRequirements] =
            requirementsViewSet(
              Iso.id.asLens,
              _ match
                case s @ ScienceRequirements.Spectroscopy(_, _, _, _, _, focalPlane, _, _) =>
                  UpdateScienceRequirements.spectroscopyRequirements(s)
            )

          val spectroscopyView: ViewOpt[Spectroscopy] =
            requirementsView.zoom(ScienceRequirements.spectroscopy)

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
                    agsState,
                    props.readonly
                  )
                ),
              if (optModeView.get.isEmpty)
                props.obsConf.constraints
                  .map(constraints =>
                    BasicConfigurationPanel(
                      props.userId,
                      props.obsId,
                      spectroscopyView,
                      props.selectedConfig,
                      constraints,
                      props.itcTargets,
                      props.baseCoordinates,
                      props.obsConf.calibrationRole,
                      createConfiguration(
                        props.obsId,
                        props.selectedConfig.get.flatMap(_.toBasicConfiguration),
                        optModeView
                      ),
                      props.modes,
                      props.readonly,
                      props.units
                    )
                  )
              else
                React.Fragment(
                  // Gmos North Long Slit
                  (optNorthAligner, spectroscopyView.asView).mapN((northAligner, specView) =>
                    AdvancedConfigurationPanel
                      .GmosNorthLongSlit(
                        props.programId,
                        props.obsId,
                        props.obsConf.calibrationRole,
                        northAligner,
                        specView,
                        revertConfiguration,
                        props.modes,
                        props.sequenceChanged,
                        props.readonly,
                        props.units
                      )
                  ),
                  // Gmos South Long Slit
                  (optSouthAligner, spectroscopyView.asView).mapN((southAligner, specView) =>
                    AdvancedConfigurationPanel
                      .GmosSouthLongSlit(
                        props.programId,
                        props.obsId,
                        props.obsConf.calibrationRole,
                        southAligner,
                        specView,
                        revertConfiguration,
                        props.modes,
                        props.sequenceChanged,
                        props.readonly,
                        props.units
                      )
                  )
                )
            )
          )

        }
