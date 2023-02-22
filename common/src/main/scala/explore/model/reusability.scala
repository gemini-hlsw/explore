// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import clue.PersistentClientStatus
import explore.data.KeyedIndexedList
import explore.events.CatalogMessage
import explore.model.Asterism
import explore.model.enums.AgsState
import explore.model.enums.SelectedPanel
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.ItcTarget
import explore.modes.InstrumentRow
import explore.undo.UndoStacks
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.AgsPosition
import lucuma.ags.GuideStarCandidate
import lucuma.catalog.AngularSize
import lucuma.catalog.CatalogTargetResult
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.IntPercent
import lucuma.core.model.ObjectTracking
import lucuma.core.model.Partner
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.model.*
import lucuma.ui.reusability.given
import queries.schemas.odb.ObsQueries.ObsSummariesWithConstraints
import queries.schemas.odb.ObsQueries.SpectroscopyRequirementsData

import scala.collection.immutable.SortedMap
import scala.collection.immutable.TreeSeqMap

/**
 * Reusability instances for model classes
 */
object reusability:
  // Model
  given Reusability[ItcTarget]                                                = Reusability.byEq
  given Reusability[PersistentClientStatus]                                   = Reusability.byEq
  given Reusability[TargetVisualOptions]                                      = Reusability.byEq
  given Reusability[UserVault]                                                = Reusability.byEq
  given Reusability[ExpandedIds]                                              = Reusability.byEq
  given Reusability[RootModel]                                                = Reusability.byEq
  given idListReuse[Id, A: Reusability]: Reusability[KeyedIndexedList[Id, A]] =
    Reusability.by(_.toList)

  given Reusability[TreeSeqMap[Target.Id, Target]] =
    Reusability.by((_: TreeSeqMap[Target.Id, Target]).toMap)(Reusability.map)
  given Reusability[ObsIdSet]                      = Reusability.byEq
  given Reusability[TargetIdSet]                   = Reusability.byEq
  given Reusability[TargetWithId]                  = Reusability.byEq
  given Reusability[TargetWithIdAndObs]            = Reusability.byEq
  given Reusability[TargetWithObs]                 = Reusability.byEq
  given Reusability[ConstraintsSummary]            = Reusability.byEq
  given Reusability[ConstraintGroup]               = Reusability.byEq
  given Reusability[ObsSummary]                    = Reusability.byEq
  given Reusability[ExploreLocalPreferences]       = Reusability.byEq

  /**
   */
  given Reusability[PosAngleConstraint]                    = Reusability.byEq
  given Reusability[ObsSummaryWithConstraints]             =
    Reusability.byEq
  given Reusability[ObsSummaryWithTitleAndConstraints]     = Reusability.byEq
  given Reusability[ObsSummaryWithTitleConstraintsAndConf] = Reusability.byEq

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
  given modelUndoStacksReuse[F[_]]: Reusability[ModelUndoStacks[F]]           = Reusability.byEq

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

  given Reusability[Existence]                    = Reusability.byEq
  given Reusability[SpectroscopyRequirementsData] = Reusability.byEq

  given Reusability[ItcChartExposureTime] = Reusability.byEq

  given Reusability[InstrumentRow] = Reusability.byEq

  given Reusability[CentralWavelength] = Reusability.byEq

  given Reusability[ObjectTracking] = Reusability.byEq

  given Reusability[Asterism] = Reusability.byEq[Asterism]

  given Reusability[TargetWithOptId] = Reusability.byEq

  given Reusability[UserGlobalPreferences] = Reusability.byEq

  given Reusability[ObsSummariesWithConstraints] = Reusability.byEq

  given Reusability[SelectedPanel] = Reusability.byEq

  given Reusability[TimingWindowRepeat] = Reusability.byEq

  given Reusability[TimingWindow] = Reusability.byEq

  given Reusability[Visit] = Reusability.byEq

  given Reusability[StepRecord] = Reusability.byEq
