// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.Async
import cats.syntax.all._
import clue.TransactionalClient
import explore.common.TargetListGroupQueries
import explore.common.TargetListGroupQueries._
import explore.implicits._
import explore.model.SelectedPanel
import explore.model.SelectedPanel.Editor
import explore.undo._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.TargetEnvironment
import lucuma.schemas.ObservationDB

import scala.collection.immutable.SortedSet
import cats.data.NonEmptySet
import explore.model.TargetEnv
import scala.collection.immutable.TreeSeqMap
import explore.model.TargetIdSet
import explore.model.implicits._
import cats.Order._

object TargetListGroupObsListActions {
  private def getter(
    targetEnvId: TargetEnvironment.Id
  ): TargetListGroupList => Option[TreeSeqMap[TargetIdSet, Target]] =
    _.values.find(_.targetEnvIds.contains(targetEnvId)).map(_.scienceTargets)

  private def setter(obsId: Observation.Id, targetEnvId: TargetEnvironment.Id)(
    otl:                    Option[TreeSeqMap[TargetIdSet, Target]]
  ): TargetListGroupList => TargetListGroupList = tlgl => {
    val targetListGroups = tlgl.values

    val updatedTlgl =
      targetListGroups.find(_.targetEnvIds.contains(targetEnvId)).fold(tlgl) { oldTlg =>
        val newList = tlgl - oldTlg.obsIds
        // Even if there are not observation ids left, it may still be an "unmoored" target list.
        // We'll determine that by seeing if there is more than one target environment id, although
        // this may be out of date while awaiting for a server round trip.
        if (oldTlg.targetEnvIds.length > 1)
          newList + oldTlg.removeId((targetEnvId, obsId.some)).asObsKeyValue
        else newList
      }

    otl.fold(updatedTlgl) { tl =>
      targetListGroups
        .find(_.scienceTargets === tl)
        .fold(
          updatedTlgl + TargetEnv(NonEmptySet.one((targetEnvId, obsId.some)), tl).asObsKeyValue
        ) { newTlg =>
          updatedTlgl - newTlg.obsIds + newTlg.addId((targetEnvId, obsId.some)).asObsKeyValue
        }
    }
  }

  private def updateExpandedIds(
    targetEnvId: TargetEnvironment.Id,
    destIds:     NonEmptySet[TargetEnvironment.Id]
  )(
    eids:        SortedSet[NonEmptySet[TargetEnvironment.Id]]
  ) =
    eids.map(ids =>
      if (ids === destIds) destIds.add(targetEnvId)
      else NonEmptySet.fromSetUnsafe(ids - targetEnvId) // TODO Deal with possible empty set?
    ) + (destIds.add(targetEnvId)) // always expand the destination, even if it wasn't expanded.

  private def updateSelected(
    targetEnvId: TargetEnvironment.Id,
    destIds:     NonEmptySet[TargetEnvironment.Id]
  )(
    selected:    SelectedPanel[NonEmptySet[TargetEnvironment.Id]]
  ) =
    selected match {
      // If in edit mode, always edit the destination.
      case Editor(_) => Editor(destIds.add(targetEnvId))
      case _         => selected
    }

  def obsTargetListGroup[F[_]](
    obsId:          Observation.Id,
    targetEnvId:    TargetEnvironment.Id,
    expandedIds:    View[SortedSet[NonEmptySet[TargetEnvironment.Id]]],
    selected:       View[SelectedPanel[NonEmptySet[TargetEnvironment.Id]]]
  )(implicit async: Async[F], c: TransactionalClient[F, ObservationDB]) =
    Action[F](getter = getter(targetEnvId), setter = setter(obsId, targetEnvId))(
      onSet = (tlgl, otl) =>
        otl.fold(async.unit) { tl =>
          // Should always find the destIds, but...
          // Need to find them here rather that pass them in so that it
          // works for undo.
          val destIds = tlgl.values
            .find(_.scienceTargets === tl)
            .map(_.targetEnvIds)
            .get //OrElse(SortedSet.empty[TargetEnvironment.Id])
          TargetListGroupQueries.replaceObservationScienceTargetList[F](obsId, tl.values.toList) >>
            expandedIds.mod(updateExpandedIds(targetEnvId, destIds) _).to[F] >>
            selected.mod(updateSelected(targetEnvId, destIds) _).to[F]
        }
    )
}
