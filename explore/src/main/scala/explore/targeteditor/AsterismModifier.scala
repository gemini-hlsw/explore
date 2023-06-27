// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.*
import explore.DefaultErrorPolicy
import explore.common.AsterismQueries
import explore.common.TargetQueries
import explore.model.AsterismIds
import explore.model.ObsIdSet
import explore.model.TargetList
import explore.utils.ToastCtx
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.model.TargetWithOptId
import lucuma.schemas.odb.input.*
import org.typelevel.log4cats.Logger

trait AsterismModifier:

  // In the future, we could unify all this into an operation over ProgramSummaries,
  // and add undo.
  // We have to be careful with undo, though. The inserted target could be in use in
  // another asterism by the time we undo its creation. What happens then?
  // Can the DB handle this (ie: keep the target if it's in use)?
  // Does the DB remove it from all asterisms?
  // If we keep it, what happens if we redo?
  protected def insertSiderealTarget(
    programId:       Program.Id,
    obsIds:          ObsIdSet,
    asterismIds:     View[AsterismIds],
    allTargets:      View[TargetList],
    targetWithOptId: TargetWithOptId
  )(using FetchClient[IO, ObservationDB], Logger[IO], ToastCtx[IO]): IO[Option[Target.Id]] =
    targetWithOptId match
      case TargetWithOptId(oTargetId, target @ Target.Sidereal(_, _, _, _)) =>
        val targetId: IO[Target.Id] = oTargetId.fold(
          TargetQueries
            .insertTarget[IO](programId, target)
            .flatTap(id => allTargets.async.mod(_ + (id -> target)))
        )(IO(_))

        targetId
          .flatTap(tid =>
            AsterismQueries.addTargetsToAsterisms[IO](programId, obsIds.toList, List(tid))
          )
          .flatTap(tid => asterismIds.async.mod(_ + tid))
          .map(_.some)
      case _                                                                =>
        IO(none)
