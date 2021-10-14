// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.Async
import cats.implicits._
import clue.TransactionalClient
import explore.common.TargetListGroupQueries
import explore.common.TargetListGroupQueries._
import explore.implicits._
import explore.model.SelectedPanel
import explore.model.SelectedPanel.Editor
import explore.model.TargetListGroup
import explore.undo._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.TargetEnvironment
import lucuma.schemas.ObservationDB

import scala.collection.immutable.SortedSet

object TargetListGroupObsListActions {
  private def getter(
    targetEnvId: TargetEnvironment.Id
  ): TargetListGroupList => Option[List[Target]] =
    _.values.find(_.targetEnvIds.contains(targetEnvId)).map(_.targets)

  private def setter(obsId: Observation.Id, targetEnvId: TargetEnvironment.Id)(
    otl:                    Option[List[Target]]
  ): TargetListGroupList => TargetListGroupList = tlgl => {
    val targetListGroups = tlgl.values

    val updatedTlgl =
      targetListGroups.find(_.targetEnvIds.contains(targetEnvId)).fold(tlgl) { oldTlg =>
        val newList = tlgl - oldTlg.obsIds
        // Even if there are not observation ids left, it may still be an "unmoored" target list.
        // We'll determine that by seeing if there is more than one target environment id, although
        // this may be out of date while awaiting for a server round trip.
        if (oldTlg.targetEnvIds.size > 1) newList + oldTlg.removeIds(obsId, targetEnvId).asKeyValue
        else newList
      }

    otl.fold(updatedTlgl) { tl =>
      targetListGroups
        .find(_.targets === tl)
        .fold(
          updatedTlgl + TargetListGroup(SortedSet(obsId), SortedSet(targetEnvId), tl).asKeyValue
        ) { newTlg =>
          updatedTlgl - newTlg.obsIds + newTlg.addIds(obsId, targetEnvId).asKeyValue
        }
    }
  }

  private def updateExpandedIds(
    targetEnvId: TargetEnvironment.Id,
    destIds:     SortedSet[TargetEnvironment.Id]
  )(
    eids:        SortedSet[SortedSet[TargetEnvironment.Id]]
  ) =
    eids.map(ids =>
      if (ids === destIds) destIds + targetEnvId else ids - targetEnvId
    ) + (destIds + targetEnvId) // always expand the destination, even if it wasn't expanded.

  private def updateSelected(
    targetEnvId: TargetEnvironment.Id,
    destIds:     SortedSet[TargetEnvironment.Id]
  )(
    selected:    SelectedPanel[SortedSet[TargetEnvironment.Id]]
  ) =
    selected match {
      // If in edit mode, always edit the destination.
      case Editor(_) => Editor(destIds + targetEnvId)
      case _         => selected
    }

  def obsTargetListGroup[F[_]](
    obsId:          Observation.Id,
    targetEnvId:    TargetEnvironment.Id,
    expandedIds:    View[SortedSet[SortedSet[TargetEnvironment.Id]]],
    selected:       View[SelectedPanel[SortedSet[TargetEnvironment.Id]]]
  )(implicit async: Async[F], c: TransactionalClient[F, ObservationDB]) =
    Action[F](getter = getter(targetEnvId), setter = setter(obsId, targetEnvId))(
      onSet = (tlgl, otl) =>
        otl.fold(async.unit) { tl =>
          // Should always find the destIds, but...
          // Need to find them here rather that pass them in so that it
          // works for undo.
          val destIds = tlgl.values
            .find(_.targets === tl)
            .map(_.targetEnvIds)
            .getOrElse(SortedSet.empty[TargetEnvironment.Id])
          TargetListGroupQueries.replaceObservationScienceTargetList[F](obsId, tl) >>
            expandedIds.mod(updateExpandedIds(targetEnvId, destIds) _).to[F] >>
            selected.mod(updateSelected(targetEnvId, destIds) _).to[F]
        }
    )
}
