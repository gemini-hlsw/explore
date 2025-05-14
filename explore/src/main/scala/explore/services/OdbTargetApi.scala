// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.effect.Resource
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.ObsIdSet
import explore.targets.TargetSearchResult
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.ObservationDB.Types.UpdateTargetsInput
import lucuma.schemas.model.TargetWithId
import queries.common.TargetQueriesGQL.ProgramTargetsDelta

trait OdbTargetApi[F[_]]:
  def insertTarget(programId:      Program.Id, target:         Target.Sidereal): F[Target.Id]
  def updateTarget(targetId:       Target.Id, input:           UpdateTargetsInput): F[Unit]
  def setTargetExistence(
    programId: Program.Id,
    targetId:  Target.Id,
    existence: Existence
  ): F[Unit]
  def deleteTargets(targetIds:     List[Target.Id], programId: Program.Id): F[Unit]
  def undeleteTargets(targetIds:   List[Target.Id], programId: Program.Id): F[Unit]
  def cloneTarget(
    targetId:  Target.Id,
    replaceIn: ObsIdSet,
    input:     UpdateTargetsInput
  ): F[TargetWithId]
  def targetEditSubscription(
    targetId: Target.Id
  ): Resource[F, fs2.Stream[F, Unit]]
  def programTargetsDeltaSubscription(
    programId: Program.Id
  ): Resource[F, fs2.Stream[F, ProgramTargetsDelta.Data.TargetEdit]]
  def searchTargetsByNamePrefix(
    programId: Program.Id,
    name:      NonEmptyString
  ): F[List[TargetSearchResult]]
  def allProgramTargets(programId: Program.Id): F[List[TargetWithId]]
