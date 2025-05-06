// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.MonadThrow
import explore.model.ObsIdSet
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.ObservationDB.Types.UpdateTargetsInput
import lucuma.schemas.model.TargetWithId

trait OdbTargetApi[F[+_]: MonadThrow]:
  private val Uninitialized: F[Nothing] =
    MonadThrow[F].raiseError:
      new RuntimeException("OdbApi is not initialized.")

  def insertTarget(programId: Program.Id, target: Target.Sidereal): F[Target.Id] = Uninitialized

  def updateTarget(targetId: Target.Id, input: UpdateTargetsInput): F[Unit] = Uninitialized

  def setTargetExistence(
    programId: Program.Id,
    targetId:  Target.Id,
    existence: Existence
  ): F[Unit] = Uninitialized

  def deleteTargets(targetIds: List[Target.Id], programId: Program.Id): F[Unit] = Uninitialized

  def undeleteTargets(targetIds: List[Target.Id], programId: Program.Id): F[Unit] = Uninitialized

  def cloneTarget(
    targetId:  Target.Id,
    replaceIn: ObsIdSet,
    input:     UpdateTargetsInput
  ): F[TargetWithId] = Uninitialized
