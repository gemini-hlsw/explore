// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.implicits._
import explore.model.decoders._
import io.circe.Decoder
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.TargetEnvironment
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedSet

case class TargetListGroup(
  obsIds:       SortedSet[Observation.Id],
  targetEnvIds: SortedSet[TargetEnvironment.Id],
  targets:      List[Target] // TODO: this will be a list of science targets
) {
  lazy val name: String = if (targets.isEmpty) "<No Targets>" else targets.map(_.name).mkString(";")

  def addIds(obsId: Observation.Id, targetEnvId: TargetEnvironment.Id): TargetListGroup =
    this.copy(obsIds = this.obsIds + obsId, targetEnvIds = this.targetEnvIds + targetEnvId)

  def removeIds(obsId: Observation.Id, targetEnvId: TargetEnvironment.Id): TargetListGroup =
    this.copy(obsIds = this.obsIds - obsId, targetEnvIds = this.targetEnvIds - targetEnvId)

  def asKeyValue: (SortedSet[Observation.Id], TargetListGroup) = (this.obsIds, this)
}

object TargetListGroup {
  val obsIds: Lens[TargetListGroup, SortedSet[Observation.Id]]             = Focus[TargetListGroup](_.obsIds)
  val targetEnvIds: Lens[TargetListGroup, SortedSet[TargetEnvironment.Id]] =
    Focus[TargetListGroup](_.targetEnvIds)
  val targets: Lens[TargetListGroup, List[Target]]                         = Focus[TargetListGroup](_.targets)

  implicit val eqTargetListGroup: Eq[TargetListGroup] =
    Eq.by(x => (x.obsIds, x.targetEnvIds, x.targets))

  implicit val decoderTargetListGroup: Decoder[TargetListGroup] = Decoder.instance(c =>
    for {
      obsIds       <- c.downField("observationIds").as[SortedSet[Observation.Id]]
      targetEnvIds <- c.downField("targetEnvironmentIds").as[SortedSet[TargetEnvironment.Id]]
      targets      <- c.downField("commonTargetList").as[List[Target]]
    } yield TargetListGroup(obsIds, targetEnvIds, targets)
  )
}
