// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Order._
import cats.data.NonEmptySet
import cats.effect.Async
import cats.syntax.all._
import clue.TransactionalClient
import explore.common.TargetListGroupQueries
import explore.common.TargetListGroupQueries._
import explore.implicits._
import explore.model.SelectedPanel
import explore.model.SelectedPanel.Editor
import explore.model.TargetEnv
import explore.model.TargetEnvIdObsId
import explore.model.TargetEnvIdObsIdSet
import explore.model.TargetIdSet
import explore.model.implicits._
import explore.undo._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.TargetEnvironment
import lucuma.schemas.ObservationDB

import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSeqMap

object TargetListGroupObsListActions {
  private def getter(
    targetEnvObsId: TargetEnvIdObsId
  ): TargetListGroupList => Option[TreeSeqMap[TargetIdSet, Target]] =
    _.values.find(_.id.contains(targetEnvObsId)).map(_.scienceTargets)

  private def setter(targetEnvObsId: TargetEnvIdObsId)(
    otl:                             Option[TreeSeqMap[TargetIdSet, Target]]
  ): TargetListGroupList => TargetListGroupList = tlgl => {
    val targetListGroups = tlgl.values

    val updatedTlgl =
      targetListGroups.find(_.id.contains(targetEnvObsId)).fold(tlgl) { oldTlg =>
        val newList = tlgl - oldTlg.id
        // Even if there are not observation ids left, it may still be an "unmoored" target list.
        // We'll determine that by seeing if there is more than one target environment id, although
        // this may be out of date while awaiting for a server round trip.
        if (oldTlg.targetEnvIds.length > 1)
          newList + oldTlg.removeId(targetEnvObsId).asObsKeyValue
        else newList
      }

    otl.fold(updatedTlgl) { tl =>
      targetListGroups
        .find(_.scienceTargets === tl)
        .fold(
          updatedTlgl + TargetEnv(NonEmptySet.one(targetEnvObsId), tl).asObsKeyValue
        ) { newTlg =>
          updatedTlgl - newTlg.id + newTlg.addId(targetEnvObsId).asObsKeyValue
        }
    }
  }

  private def updateExpandedIds(
    targetEnvObsId: TargetEnvIdObsId,
    optDestIds:     Option[TargetEnvIdObsIdSet]
  )(
    eids:           SortedSet[TargetEnvIdObsIdSet]
  ) = {
    val setOfOne: TargetEnvIdObsIdSet = NonEmptySet.one(targetEnvObsId)

    optDestIds.fold(
      eids.map(ids =>
        if (ids =!= setOfOne) NonEmptySet.fromSetUnsafe(ids -- setOfOne)
        else ids
      ) + setOfOne
    ) { destIds =>
      eids.flatMap(ids =>
        if (ids === destIds || ids === setOfOne) none
        else NonEmptySet.fromSetUnsafe(ids - targetEnvObsId).some
      ) + destIds.add(targetEnvObsId)
    }
  }

  private def updateSelected(
    targetEnvObsId: TargetEnvIdObsId,
    optDestIds:     Option[TargetEnvIdObsIdSet]
  )(
    selected:       SelectedPanel[TargetEnvIdObsIdSet]
  ) =
    selected match {
      // If in edit mode, always edit the destination.
      case Editor(_) =>
        Editor(optDestIds.fold(NonEmptySet.one(targetEnvObsId))(_.add(targetEnvObsId)))
      case _         => selected
    }

  def obsTargetListGroup[F[_]](
    obsId:          Observation.Id,
    targetEnvId:    TargetEnvironment.Id,
    expandedIds:    View[SortedSet[TargetEnvIdObsIdSet]],
    selected:       View[SelectedPanel[TargetEnvIdObsIdSet]]
  )(implicit async: Async[F], c: TransactionalClient[F, ObservationDB]) = {
    val targetEnvObsId: TargetEnvIdObsId = (targetEnvId, obsId.some)
    Action[F](getter = getter(targetEnvObsId), setter = setter(targetEnvObsId))(
      onSet = (tlgl, otl) =>
        otl.fold(async.unit) { tl =>
          // destination ids may not be found when undoing
          val optDestIds = tlgl.values.find(_.scienceTargets === tl).map(_.id)
          TargetListGroupQueries.replaceObservationScienceTargetList[F](obsId, tl.values.toList) >>
            expandedIds.mod(updateExpandedIds(targetEnvObsId, optDestIds) _).to[F] >>
            selected.mod(updateSelected(targetEnvObsId, optDestIds) _).to[F]
        }
    )
  }
}
