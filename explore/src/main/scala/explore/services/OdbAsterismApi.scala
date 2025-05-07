// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import lucuma.core.model.Observation
import lucuma.core.model.Target

trait OdbAsterismApi[F[_]]:
  def replaceAsterism(obsIds: List[Observation.Id], targetIds: List[Target.Id]): F[Unit]

  def addTargetsToAsterisms(obsIds: List[Observation.Id], targetIds: List[Target.Id]): F[Unit]

  def removeTargetsFromAsterisms(
    obsIds:    List[Observation.Id],
    targetIds: List[Target.Id]
  ): F[Unit]

  def addAndRemoveTargetsFromAsterisms(
    obsIds:   List[Observation.Id],
    toAdd:    List[Target.Id],
    toRemove: List[Target.Id]
  ): F[Unit]

  def undeleteTargetsAndAddToAsterism(
    obsIds:    List[Observation.Id],
    targetIds: List[Target.Id]
  ): F[Unit]

  def deleteTargetsAndRemoveFromAsterism(
    obsIds:    List[Observation.Id],
    targetIds: List[Target.Id]
  ): F[Unit]
