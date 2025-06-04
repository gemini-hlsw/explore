// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Order.given
import cats.effect.IO
import cats.syntax.all.*
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
import explore.modes.ConfigSelection
import explore.modes.ItcInstrumentConfig
import explore.modes.ScienceModes
import explore.services.OdbObservationApi
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
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.DropdownOptional
import lucuma.react.primereact.SelectItem
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import monocle.Iso
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
    selectedConfig:           View[ConfigSelection],
    revertedInstrumentConfig: List[ItcInstrumentConfig], // configuration rows selected if reverted
    modes:                    ScienceModes,
    customSedTimestamps:      List[Timestamp],
    allTargets:               TargetList,
    observingModeGroups:      ObservingModeGroupList,
    sequenceChanged:          Callback,
    readonly:                 Boolean,
    units:                    WavelengthUnits,
    isStaff:                  Boolean
  ) =
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
          customSedTimestamps,
          sequenceChanged,
          readonly,
          units,
          isStaff
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
    obsId:  Observation.Id
  )(using
    odbApi: OdbObservationApi[IO]
  ): Action[PosAngleConstraintAndObsMode, PosAngleConstraintAndObsMode] =
    Action[PosAngleConstraintAndObsMode, PosAngleConstraintAndObsMode](
      getter = identity,
      setter = x => _ => x
    )(
      onSet = (_, _) => IO.unit, // Nothing to do, creation is done before this
      onRestore = (_, pm) =>
        val (pac, oMode) = pm

        odbApi.updateObservations(
          List(obsId),
          ObservationPropertiesInput(
            observingMode = oMode.map(_.toInput).orUnassign,
            posAngleConstraint = pac.toInput.assign
          )
        )
    )
  private def modeAction(
    obsId:  Observation.Id
  )(using
    odbApi: OdbObservationApi[IO]
  ): Action[Option[ObservingMode], Option[ObservingMode]] =
    Action[Option[ObservingMode], Option[ObservingMode]](
      getter = identity,
      setter = x => _ => x
    )(
      onSet = (_, _) => IO.unit, // Nothing to do, creation is done before this
      onRestore = (_, oMode) =>
        odbApi.updateObservations(
          List(obsId),
          ObservationPropertiesInput(observingMode = oMode.map(_.toInput).orUnassign)
        )
    )

  private def updateConfiguration(
    obsId:                    Observation.Id,
    pacAndMode:               UndoSetter[PosAngleConstraintAndObsMode],
    input:                    ObservingModeInput,
    defaultPosAngleConstrait: PosAngleOptions
  )(using odbApi: OdbObservationApi[IO]): IO[Unit] =
    val currentPac = pacAndMode.get._1
    if (defaultPosAngleConstrait != currentPac.toPosAngleOptions)
      val angle  =
        PosAngleConstraint.angle.getOption(currentPac).getOrElse(Angle.Angle0)
      val newPac = defaultPosAngleConstrait.toPosAngle(angle)
      odbApi
        .updateConfiguration(obsId, input.assign, newPac.toInput.assign)
        .flatMap: om =>
          pacAndModeAction(obsId)
            .set(pacAndMode)((newPac, om))
            .toAsync
    else
      odbApi
        .updateConfiguration(obsId, input.assign)
        .flatMap: om =>
          modeAction(obsId)
            .set(pacAndMode.zoom(PosAngleConstraintAndObsMode.observingMode))(om)
            .toAsync

  private def revertConfiguration(
    obsId:                    Observation.Id,
    mode:                     UndoSetter[Option[ObservingMode]],
    revertedInstrumentConfig: List[ItcInstrumentConfig],
    selectedConfig:           View[ConfigSelection]
  )(using odbApi: OdbObservationApi[IO]): IO[Unit] =
    odbApi
      .updateConfiguration(obsId, Input.unassign) >>
      (modeAction(obsId).set(mode)(none) >>
        selectedConfig.set(ConfigSelection.fromInstrumentConfigs(revertedInstrumentConfig))).toAsync

  private case class Body(
    userId:                   Option[User.Id],
    programId:                Program.Id,
    obsId:                    Observation.Id,
    requirements:             UndoSetter[ScienceRequirements],
    pacAndMode:               UndoSetter[PosAngleConstraintAndObsMode],
    obsConf:                  ObsConfiguration,
    itcTargets:               List[ItcTarget],
    baseCoordinates:          Option[CoordinatesAtVizTime],
    selectedConfig:           View[ConfigSelection],
    revertedInstrumentConfig: List[ItcInstrumentConfig],
    modes:                    ScienceModes,
    customSedTimestamps:      List[Timestamp],
    sequenceChanged:          Callback,
    readonly:                 Boolean,
    units:                    WavelengthUnits,
    isStaff:                  Boolean
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
      ScalaFnComponent[Props] { props =>
        for
          ctx <- useContext(AppContext.ctx)
          _   <- useEffectWithDeps(props.requirements.get.scienceModeType): modeType =>
                   // if neither spectroscopy or imaging is set, we want to update the observation
                   // to default to spectroscopy, but only locally. Sending an empty spectroscopy
                   // to the API is the same as setting it to neither spec or imaging.
                   modeType.fold(
                     props.requirements.model // using the `model` excludes it from undo/redo
                       .zoom(ScienceRequirements.scienceMode)
                       .set(Spectroscopy.Default.asLeft.some)
                   )(_ => Callback.empty)
        yield
          import ctx.given

          val posAngleConstraintAligner
            : Aligner[PosAngleConstraint, Input[PosAngleConstraintInput]] =
            Aligner(
              props.posAngle,
              UpdateObservationsInput(
                WHERE = props.obsId.toWhereObservation.assign,
                SET = ObservationPropertiesInput()
              ),
              ctx.odbApi.updateObservations(_)
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
              (ctx.odbApi.updateObservations(_)).andThen(_.void)
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

          val optGmosNorthAligner = optModeAligner.flatMap {
            _.zoomOpt(
              ObservingMode.gmosNorthLongSlit,
              modOrAssignAndMap(GmosNorthLongSlitInput())(
                ObservingModeInput.gmosNorthLongSlit.modify
              )
            )
          }

          val optGmosSouthAligner = optModeAligner.flatMap {
            _.zoomOpt(
              ObservingMode.gmosSouthLongSlit,
              modOrAssignAndMap(GmosSouthLongSlitInput())(
                ObservingModeInput.gmosSouthLongSlit.modify
              )
            )
          }

          val optFlamingos2Aligner = optModeAligner.flatMap {
            _.zoomOpt(
              ObservingMode.f2LongSlit,
              modOrAssignAndMap(Flamingos2LongSlitInput())(
                ObservingModeInput.flamingos2LongSlit.modify
              )
            )
          }

          val requirementsViewSet: ScienceRequirementsUndoView =
            ScienceRequirementsUndoView(props.obsId, props.requirements)

          val exposureTimeModeView =
            props.requirements.zoom(ScienceRequirements.exposureTimeMode)

          val requirementsView: View[ScienceRequirements] =
            requirementsViewSet(
              Iso.id.asLens,
              UpdateScienceRequirements.scienceRequirements
            )

          val spectroscopyView: ViewOpt[Spectroscopy] = requirementsView
            .zoom(ScienceRequirements.spectroscopy)

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
                      requirementsView,
                      props.selectedConfig,
                      constraints,
                      props.itcTargets,
                      props.baseCoordinates,
                      props.obsConf.calibrationRole,
                      props.selectedConfig.get
                        .toBasicConfiguration()
                        .map: bc =>
                          updateConfiguration(props.obsId,
                                              props.pacAndMode,
                                              bc.toInput,
                                              bc.obsModeType.defaultPosAngleOptions
                          )
                        .orEmpty,
                      props.modes,
                      props.customSedTimestamps,
                      props.readonly,
                      props.units
                    )
                  )
              else
                React.Fragment(
                  // Gmos North Long Slit
                  (optGmosNorthAligner, spectroscopyView.asView).mapN((northAligner, specView) =>
                    GmosLongslitConfigPanel
                      .GmosNorthLongSlit(
                        props.programId,
                        props.obsId,
                        props.obsConf.calibrationRole,
                        northAligner,
                        exposureTimeModeView.model,
                        specView,
                        revertConfig,
                        props.modes.spectroscopy,
                        props.sequenceChanged,
                        props.readonly,
                        props.units
                      )
                  ),
                  // Gmos South Long Slit
                  (optGmosSouthAligner, spectroscopyView.asView).mapN((southAligner, specView) =>
                    GmosLongslitConfigPanel
                      .GmosSouthLongSlit(
                        props.programId,
                        props.obsId,
                        props.obsConf.calibrationRole,
                        southAligner,
                        exposureTimeModeView.model,
                        specView,
                        revertConfig,
                        props.modes.spectroscopy,
                        props.sequenceChanged,
                        props.readonly,
                        props.units
                      )
                  ),
                  // Flamingos2 Long Slit
                  (optFlamingos2Aligner, spectroscopyView.asView).mapN((f2Aligner, specView) =>
                    Flamingos2LongslitConfigPanel(
                      props.programId,
                      props.obsId,
                      props.obsConf.calibrationRole,
                      f2Aligner,
                      exposureTimeModeView.model,
                      specView,
                      revertConfig,
                      props.modes.spectroscopy,
                      props.sequenceChanged,
                      props.readonly,
                      props.units,
                      props.isStaff
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
    selectedConfig:           View[ConfigSelection],
    revertedInstrumentConfig: List[ItcInstrumentConfig],
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
                                    m.obsModeType.defaultPosAngleOptions
                ).switching(isChanging.async).runAsync
              ),
            options = props.observingModeGroups.values.toList.sorted
              .map:
                _.map: om =>
                  new SelectItem[ObservingModeSummary](value = om, label = om.shortName)
              .flattenOption
          ).unless(props.readonly)
        )
