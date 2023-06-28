// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.kernel.Eq
import clue.PersistentClientStatus
import explore.data.KeyedIndexedList
import explore.events.CatalogMessage
import explore.model.enums.AgsState
import explore.model.enums.SelectedPanel
import explore.model.itc.ItcExposureTime
import explore.model.itc.ItcTarget
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
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.IntPercent
import lucuma.core.model.ObjectTracking
import lucuma.core.model.Partner
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.sequence.Atom
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
  given Reusability[TargetVisualOptions]                                      = Reusability.byEq
  given Reusability[UserVault]                                                = Reusability.byEq
  given Reusability[ExpandedIds]                                              = Reusability.byEq
  given Reusability[RootModel]                                                = Reusability.byEq
  given idListReuse[Id, A: Reusability]: Reusability[KeyedIndexedList[Id, A]] =
    Reusability.by(_.toList)

  given Reusability[ObsIdSet]                = Reusability.byEq
  given Reusability[TargetIdSet]             = Reusability.byEq
  given Reusability[TargetWithId]            = Reusability.byEq
  given Reusability[TargetWithIdAndObs]      = Reusability.byEq
  given Reusability[TargetWithObs]           = Reusability.byEq
  given Reusability[ConstraintGroup]         = Reusability.byEq
  given Reusability[ObsSummary]              = Reusability.byEq
  given Reusability[ExploreLocalPreferences] = Reusability.byEq
  given Reusability[ObsAttachment]           = Reusability.byEq
  given Reusability[ProposalAttachment]      = Reusability.byEq
  given Reusability[ProgramInfo]             = Reusability.byEq

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

  given Reusability[AvailableFilter]             = Reusability.byEq
  given Reusability[ImagingConfigurationOptions] = Reusability.byEq
  given Reusability[Progress]                    = Reusability.byEq

  given Reusability[AngularSize]         = Reusability.byEq
  given Reusability[CatalogTargetResult] = Reusability.byEq
  given Reusability[ObservingMode]       = Reusability.byEq
  given Reusability[BasicConfiguration]  = Reusability.byEq
  given Reusability[BasicConfigAndItc]   = Reusability.byEq
  given Reusability[GuideStarCandidate]  = Reusability.by(_.name.value)
  given Reusability[AgsPosition]         = Reusability.byEq
  given Reusability[AgsParams]           = Reusability.byEq
  given Reusability[AgsState]            = Reusability.byEq
  given Reusability[AgsAnalysis]         = Reusability.byEq

  given Reusability[ObsConfiguration] = Reusability.byEq

  given Reusability[Existence] = Reusability.byEq

  given Reusability[ItcExposureTime] = Reusability.byEq

  given Reusability[InstrumentRow] = Reusability.byEq

  given Reusability[CentralWavelength] = Reusability.byEq

  given Reusability[ObjectTracking] = Reusability.byEq

  given Reusability[Asterism] = Reusability.byEq[Asterism]

  given Reusability[TargetWithOptId] = Reusability.byEq

  given Reusability[UserGlobalPreferences] = Reusability.byEq

  given Reusability[SelectedPanel] = Reusability.byEq

  given Reusability[TimingWindow] = Reusability.byEq

  given Reusability[Visit] = Reusability.byEq

  given Reusability[StepRecord] = Reusability.byEq

  given Reusability[ApiKey] = Reusability.byEq

  given Reusability[SignalToNoise] = Reusability.byEq

  given Reusability[ScienceRequirements.Spectroscopy] = Reusability.byEq

  given Reusability[ScienceRequirements] = Reusability.byEq

  given Reusability[OdbItcResult.Success] = Reusability.byEq

  given Reusability[Transformation] = Reusability.byEq

  given [F[_]]: Reusability[OdbRestClient[F]] = Reusability.by(_.authToken)

  given [D: Eq]: Reusability[Atom[D]] = Reusability.byEq
