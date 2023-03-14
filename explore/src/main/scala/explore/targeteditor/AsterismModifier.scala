// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.View
import crystal.react.implicits.*
import explore.DefaultErrorPolicy
import explore.common.TargetQueries
import explore.common.AsterismQueries
import explore.model.Asterism
import explore.model.ObsIdSet
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.model.TargetWithOptId
import lucuma.schemas.odb.input.*
import org.typelevel.log4cats.Logger
import explore.utils.ToastRefF

trait AsterismModifier:
  protected def insertSiderealTarget(
    programId:       Program.Id,
    obsIds:          ObsIdSet,
    asterism:        View[Option[Asterism]],
    targetWithOptId: TargetWithOptId,
    toastRef:        ToastRefF[IO]
  )(using FetchClient[IO, ?, ObservationDB], Logger[IO]): IO[Option[Target.Id]] =
    targetWithOptId match
      case TargetWithOptId(oTargetId, target @ Target.Sidereal(_, _, _, _)) =>
        val targetId: IO[Target.Id] = oTargetId.fold(
          TargetQueries.insertTarget(programId, target, toastRef)
        )(IO(_))

        targetId
          .flatTap(tid =>
            AsterismQueries.addTargetsToAsterisms[IO](programId, obsIds.toList, List(tid))
          )
          .flatTap { tid =>
            val newTarget = TargetWithId(tid, target)

            asterism.async.mod {
              case a @ Some(_) => a.map(_.add(newTarget))
              case _           => Asterism.one(newTarget).some
            }
          }
          .map(_.some)
      case _                                                                =>
        IO(none)
