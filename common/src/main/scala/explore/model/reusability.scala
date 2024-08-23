// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyChain
import clue.PersistentClientStatus
import explore.data.KeyedIndexedList
import explore.model.IsActive
import explore.model.enums.AgsState
import explore.model.enums.SelectedPanel
import explore.model.itc.ItcExposureTime
import explore.model.itc.ItcTarget
import explore.modes.InstrumentOverrides
import explore.modes.InstrumentRow
import explore.undo.UndoStacks
import explore.utils.OdbRestClient
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.AgsPosition
import lucuma.ags.GuideStarCandidate
import lucuma.catalog.AngularSize
import lucuma.catalog.CatalogTargetResult
import lucuma.core.math.SignalToNoise
import lucuma.core.model.ObjectTracking
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.TimingWindow
import lucuma.core.model.sequence.Atom
import lucuma.itc.ItcCcd
import lucuma.itc.client.GraphResult
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.model.*
import lucuma.ui.reusability.given

/**
 * Reusability instances for model classes
 */
object reusability:
  // Model
  given Reusability[ProgramSummaries]                                         = Reusability.byEq
  given Reusability[ItcTarget]                                                = Reusability.byEq
  given Reusability[PersistentClientStatus]                                   = Reusability.byEq
  given Reusability[AsterismVisualOptions]                                    = Reusability.byEq
  given Reusability[ExpandedIds]                                              = Reusability.byEq
  given Reusability[RootModel]                                                = Reusability.byEq
  given idListReuse[Id, A: Reusability]: Reusability[KeyedIndexedList[Id, A]] =
    Reusability.by(_.toList)

  given Reusability[ObsIdSet]                = Reusability.byEq
  given Reusability[TargetEditObsInfo]       = Reusability.byEq
  given Reusability[TargetIdSet]             = Reusability.byEq
  given Reusability[TargetWithId]            = Reusability.byEq
  given Reusability[TargetWithIdAndObs]      = Reusability.byEq
  given Reusability[TargetWithObs]           = Reusability.byEq
  given Reusability[ConstraintGroup]         = Reusability.byEq
  given Reusability[Observation]             = Reusability.byEq
  given Reusability[ExploreLocalPreferences] = Reusability.byEq
  given Reusability[ObsAttachment]           = Reusability.byEq
  given Reusability[ProposalAttachment]      = Reusability.byEq
  given Reusability[ProgramInfo]             = Reusability.byEq
  given Reusability[ProgramDetails]          = Reusability.byEq
  given Reusability[Execution]               = Reusability.byEq
  given Reusability[GroupTree]               = Reusability.byEq

  /**
   */
  given Reusability[PosAngleConstraint] = Reusability.byEq

  // Undo
  given undoStacksReuse[F[_], M]: Reusability[UndoStacks[F, M]] =
    Reusability.by(s => (s.undo.length, s.redo.length, s.working))

  given undoStacksMapReuse[F[_], K, M]: Reusability[Map[K, UndoStacks[F, M]]] =
    Reusability.by[Map[K, UndoStacks[F, M]], Int](_.size) && Reusability[Map[K, UndoStacks[F, M]]](
      (a, b) =>
        a.forall { case (k, stacksA) =>
          b.get(k).exists(stacksB => undoStacksReuse.test(stacksA, stacksB))
        }
    )

  given modelUndoStacksReuse[F[_]]: Reusability[ModelUndoStacks[F]] = Reusability.byEq

  given Reusability[AvailableFilter]                    = Reusability.byEq
  given Reusability[ImagingConfigurationOptions]        = Reusability.byEq
  given Reusability[Progress]                           = Reusability.byEq
  given Reusability[AngularSize]                        = Reusability.byEq
  given Reusability[CatalogTargetResult]                = Reusability.byEq
  given Reusability[ObservingMode]                      = Reusability.byEq
  given Reusability[BasicConfiguration]                 = Reusability.byEq
  given Reusability[BasicConfigAndItc]                  = Reusability.byEq
  given Reusability[GuideStarCandidate]                 = Reusability.by(_.name.value)
  given Reusability[AgsPosition]                        = Reusability.byEq
  given Reusability[AgsParams]                          = Reusability.byEq
  given Reusability[AgsState]                           = Reusability.byEq
  given Reusability[AgsAnalysis]                        = Reusability.byEq
  given Reusability[ObsConfiguration]                   = Reusability.byEq
  given Reusability[Existence]                          = Reusability.byEq
  given Reusability[ItcExposureTime]                    = Reusability.byEq
  given Reusability[InstrumentRow]                      = Reusability.byEq
  given Reusability[CentralWavelength]                  = Reusability.byEq
  given Reusability[ObjectTracking]                     = Reusability.byEq
  given Reusability[Asterism]                           = Reusability.byEq[Asterism]
  given Reusability[TargetWithOptId]                    = Reusability.byEq
  given Reusability[GlobalPreferences]                  = Reusability.byEq
  given Reusability[SelectedPanel]                      = Reusability.byEq
  given Reusability[TimingWindow]                       = Reusability.byEq
  given [D: Eq]: Reusability[Visit[D]]                  = Reusability.byEq
  given [D: Eq]: Reusability[StepRecord[D]]             = Reusability.byEq
  given Reusability[ApiKey]                             = Reusability.byEq
  given Reusability[SignalToNoise]                      = Reusability.byEq
  given Reusability[ScienceRequirements.Spectroscopy]   = Reusability.byEq
  given Reusability[ScienceRequirements]                = Reusability.byEq
  // given Reusability[OdbItcResult.Success]               = Reusability.byEq
  given Reusability[Transformation]                     = Reusability.byEq
  given [F[_]]: Reusability[OdbRestClient[F]]           = Reusability.by(_.authToken)
  given [D: Eq]: Reusability[Atom[D]]                   = Reusability.byEq
  given Reusability[ExecutionVisits]                    = Reusability.byEq
  given Reusability[ProgramUserWithRole]                = Reusability.byEq
  given Reusability[CoIInvitation]                      = Reusability.byEq
  given Reusability[IsActive]                           = Reusability.byEq
  given Reusability[PAProperties]                       = Reusability.byEq
  given Reusability[GraphResult]                        = Reusability.byEq
  given Reusability[ItcCcd]                             = Reusability.byEq
  given Reusability[ElevationPlotOptions]               = Reusability.byEq
  given Reusability[PartnerSplit]                       = Reusability.byEq
  given Reusability[CallForProposal]                    = Reusability.byEq
  given Reusability[CategoryAllocationList]             = Reusability.byEq
  given Reusability[InstrumentOverrides]                = Reusability.byEq
  given [A: Reusability]: Reusability[NonEmptyChain[A]] = Reusability.by(_.toNonEmptyList)
