// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Order.given
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.Assign
import clue.data.Input
import clue.data.syntax.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
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
import explore.model.ObservingModeGroupList
import explore.model.ObservingModeSummary
import explore.model.PosAngleConstraintAndObsMode
import explore.model.ScienceRequirements
import explore.model.ScienceRequirements.Spectroscopy
import explore.model.TargetList
import explore.model.enums.PosAngleOptions
import explore.model.enums.WavelengthUnits
import explore.model.itc.ItcTarget
import explore.model.syntax.all.*
import explore.modes.InstrumentConfig
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import explore.undo.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Angle
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.syntax.display.*
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.DropdownOptional
import lucuma.react.primereact.SelectItem
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
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
    pacAndMode:               UndoSetter[PosAngleConstraintAndObsMode],
    scienceTargetIds:         AsterismIds,
    baseCoordinates:          Option[CoordinatesAtVizTime],
    obsConf:                  ObsConfiguration,
    selectedConfig:           View[Option[InstrumentConfigAndItcResult]],
    revertedInstrumentConfig: Option[InstrumentConfig], // configuration selected if reverted
    modes:                    SpectroscopyModesMatrix,
    allTargets:               TargetList,
    observingModeGroups:      ObservingModeGroupList,
    sequenceChanged:          Callback,
    readonly:                 Boolean,
    units:                    WavelengthUnits
  )(using Logger[IO]) =
    Tile(
      ObsTabTileIds.ConfigurationId.id,
      "Configuration",
      bodyClass = ExploreStyles.ConfigurationTileBody
    )(
      _ =>
        Body(
          userId,
          programId,
          obsId,
          requirements,
          pacAndMode,
          obsConf,
          scienceTargetIds.itcTargets(allTargets),
          baseCoordinates,
          selectedConfig,
          revertedInstrumentConfig,
          modes,
          sequenceChanged,
          readonly,
          units
        ),
      (_, _) =>
        Title(obsId,
              pacAndMode,
              observingModeGroups,
              selectedConfig,
              revertedInstrumentConfig,
              readonly
        )
    )

  private def pacAndModeAction(
    obsId: Observation.Id
  )(using
    FetchClient[IO, ObservationDB]
  ): Action[PosAngleConstraintAndObsMode, PosAngleConstraintAndObsMode] =
    Action[PosAngleConstraintAndObsMode, PosAngleConstraintAndObsMode](
      getter = identity,
      setter = x => _ => x
    )(
      onSet = (_, _) => IO.unit, // Nothing to do, creation is done before this
      onRestore = (_, pm) =>
        val (pac, oMode) = pm
        val input        = UpdateObservationsInput(
          WHERE = obsId.toWhereObservation.assign,
          SET = ObservationPropertiesInput(
            observingMode = oMode.map(_.toInput).orUnassign,
            posAngleConstraint = pac.toInput.assign
          )
        )
        ObsQueriesGQL.UpdateObservationMutation[IO].execute(input).void
    )
  private def modeAction(
    obsId: Observation.Id
  )(using
    FetchClient[IO, ObservationDB]
  ): Action[Option[ObservingMode], Option[ObservingMode]] =
    Action[Option[ObservingMode], Option[ObservingMode]](
      getter = identity,
      setter = x => _ => x
    )(
      onSet = (_, _) => IO.unit, // Nothing to do, creation is done before this
      onRestore = (_, oMode) =>
        val input = UpdateObservationsInput(
          WHERE = obsId.toWhereObservation.assign,
          SET = ObservationPropertiesInput(observingMode = oMode.map(_.toInput).orUnassign)
        )
        ObsQueriesGQL.UpdateObservationMutation[IO].execute(input).void
    )

  private def remoteUpdate(
    input: UpdateObservationsInput
  )(using FetchClient[IO, ObservationDB]): IO[Option[ObservingMode]] =
    ObsQueriesGQL
      .UpdateConfigurationMutation[IO]
      .execute(input)
      .raiseGraphQLErrors
      .map(_.updateObservations.observations.headOption.flatMap(_.observingMode))

  private def updateConfiguration(
    obsId:                    Observation.Id,
    pacAndMode:               UndoSetter[PosAngleConstraintAndObsMode],
    input:                    ObservingModeInput,
    defaultPosAngleConstrait: PosAngleOptions
  )(using FetchClient[IO, ObservationDB]): IO[Unit] =
    val obsInput   = UpdateObservationsInput(
      WHERE = obsId.toWhereObservation.assign,
      SET = ObservationPropertiesInput(observingMode = input.assign)
    )
    val currentPac = pacAndMode.get._1
    if (defaultPosAngleConstrait != currentPac.toPosAngleOptions)
      val angle    =
        PosAngleConstraint.angle.getOption(currentPac).getOrElse(Angle.Angle0)
      val newPac   = defaultPosAngleConstrait.toPosAngle(angle)
      val newInput = UpdateObservationsInput.SET
        .andThen(ObservationPropertiesInput.posAngleConstraint)
        .replace(newPac.toInput.assign)(obsInput)
      remoteUpdate(newInput)
        .flatMap: om =>
          pacAndModeAction(obsId)
            .set(pacAndMode)((newPac, om))
            .toAsync
    else
      remoteUpdate(obsInput)
        .flatMap: om =>
          modeAction(obsId)
            .set(pacAndMode.zoom(PosAngleConstraintAndObsMode.observingMode))(
              om
            )
            .toAsync

  private def revertConfiguration(
    obsId:                    Observation.Id,
    mode:                     UndoSetter[Option[ObservingMode]],
    revertedInstrumentConfig: Option[InstrumentConfig],
    selectedConfig:           View[Option[InstrumentConfigAndItcResult]]
  )(using FetchClient[IO, ObservationDB]): IO[Unit] =
    val obsInput = UpdateObservationsInput(
      WHERE = obsId.toWhereObservation.assign,
      SET = ObservationPropertiesInput(observingMode = Input.unassign)
    )
    remoteUpdate(obsInput) >>
      (modeAction(obsId).set(mode)(none) >>
        revertedInstrumentConfig
          .map: row => // Select the reverted config
            selectedConfig.mod: c =>
              InstrumentConfigAndItcResult(
                row,
                c.flatMap(_.itcResult.flatMap(_.toOption.map(_.asRight)))
              ).some
          .orEmpty).toAsync

  private case class Body(
    userId:                   Option[User.Id],
    programId:                Program.Id,
    obsId:                    Observation.Id,
    requirements:             UndoSetter[ScienceRequirements],
    pacAndMode:               UndoSetter[PosAngleConstraintAndObsMode],
    obsConf:                  ObsConfiguration,
    itcTargets:               List[ItcTarget],
    baseCoordinates:          Option[CoordinatesAtVizTime],
    selectedConfig:           View[Option[InstrumentConfigAndItcResult]],
    revertedInstrumentConfig: Option[InstrumentConfig],
    modes:                    SpectroscopyModesMatrix,
    sequenceChanged:          Callback,
    readonly:                 Boolean,
    units:                    WavelengthUnits
  ) extends ReactFnProps(Body.component):
    val mode: UndoSetter[Option[ObservingMode]]  =
      pacAndMode.zoom(PosAngleConstraintAndObsMode.observingMode)
    val posAngle: UndoSetter[PosAngleConstraint] =
      pacAndMode.zoom(PosAngleConstraintAndObsMode.posAngleConstraint)

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

    private val component =
      ScalaFnComponent
        .withHooks[Props]
        .useContext(AppContext.ctx)
        .render { (props, ctx) =>
          import ctx.given

          val posAngleConstraintAligner
            : Aligner[PosAngleConstraint, Input[PosAngleConstraintInput]] =
            Aligner(
              props.posAngle,
              UpdateObservationsInput(
                WHERE = props.obsId.toWhereObservation.assign,
                SET = ObservationPropertiesInput()
              ),
              ObsQueriesGQL
                .UpdateObservationMutation[IO]
                .execute(_)
                .void
            ).zoom(
              Iso.id,
              UpdateObservationsInput.SET
                .andThen(ObservationPropertiesInput.posAngleConstraint)
                .modify
            )

          val posAngleConstraintView: View[PosAngleConstraint] =
            posAngleConstraintAligner.view(_.toInput.assign)

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

          val revertConfig: Callback =
            revertConfiguration(
              props.obsId,
              props.mode,
              props.revertedInstrumentConfig,
              props.selectedConfig
            ).runAsyncAndForget

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
                case s @ ScienceRequirements.Spectroscopy(_, _, _, _, focalPlane, _, _) =>
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
                    posAngleConstraintView,
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
                      props.selectedConfig.get
                        .flatMap(_.toBasicConfiguration)
                        .map: bc =>
                          updateConfiguration(props.obsId,
                                              props.pacAndMode,
                                              bc.toInput,
                                              bc.defaultPosAngleConstrait
                          )
                        .orEmpty,
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
                        revertConfig,
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
                        revertConfig,
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

  private case class Title(
    obsId:                    Observation.Id,
    pacAndMode:               UndoSetter[PosAngleConstraintAndObsMode],
    observingModeGroups:      ObservingModeGroupList,
    selectedConfig:           View[Option[InstrumentConfigAndItcResult]],
    revertedInstrumentConfig: Option[InstrumentConfig],
    readonly:                 Boolean
  ) extends ReactFnProps(Title.component):
    val observingMode                           = pacAndMode.get._2
    val mode: UndoSetter[Option[ObservingMode]] =
      pacAndMode.zoom(PosAngleConstraintAndObsMode.observingMode)

  private object Title:
    private type Props = Title

    private val component = ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(false) // isChanging
      .render: (props, ctx, isChanging) =>
        import ctx.given

        val revertConfig: Callback =
          revertConfiguration(
            props.obsId,
            props.mode,
            props.revertedInstrumentConfig,
            props.selectedConfig
          ).runAsyncAndForget

        <.div(ExploreStyles.TileTitleConfigSelector)(
          DropdownOptional[ObservingModeSummary](
            value = props.observingMode.map(ObservingModeSummary.fromObservingMode),
            placeholder = "Choose existing observing mode...",
            disabled = isChanging.get,
            loading = isChanging.get,
            showClear = true,
            onChange = (om: Option[ObservingModeSummary]) =>
              om.fold(revertConfig)(m =>
                updateConfiguration(props.obsId,
                                    props.pacAndMode,
                                    m.toInput,
                                    m.defaultPosAngleConstrait
                ).switching(isChanging.async).runAsync
              ),
            options = props.observingModeGroups.values.toList.sorted
              .map:
                _.map: om =>
                  new SelectItem[ObservingModeSummary](value = om, label = om.shortName)
              .flattenOption
          ).unless(props.readonly)
        )
