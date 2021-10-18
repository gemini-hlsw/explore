// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptySet
import cats.syntax.all._
import explore.model.decoders._
import io.circe.Decoder
import io.circe.Decoder._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.TargetEnvironment
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSeqMap

case class TargetEnv(
  id:             TargetEnvIdSet,
  scienceTargets: TreeSeqMap[TargetIdSet, Target]
) {
  lazy val targetEnvIds: NonEmptySet[TargetEnvironment.Id] = id.map(_._1)
  lazy val obsIds: SortedSet[Observation.Id]               = id.collect { case (_, Some(obsId)) => obsId }

  lazy val name: String =
    if (scienceTargets.isEmpty) "<No Targets>"
    else scienceTargets.map(TargetWithId.name.get).mkString(";")

  def addId(newId: TargetEnvId): TargetEnv =
    this.copy(id = id.add(newId))

  def removeId(oldId: TargetEnvId): TargetEnv =
    // TODO Deal with this better. Maybe return an Option[TargetEnv] ?
    if (id.length === 1) this
    else
      this.copy(id = NonEmptySet.fromSetUnsafe(id - oldId))

  def asObsKeyValue: (SortedSet[Observation.Id], TargetEnv) = (this.obsIds, this)

}

object TargetEnv {
  implicit val eqTargetEnv: Eq[TargetEnv] = Eq.by(x => (x.id, x.scienceTargets.toMap))

  private val singleTargetIdDecoder: Decoder[TargetIdSet]       =
    Decoder.instance(_.get[Target.Id]("id").map(id => NonEmptySet.one(id)))
  private val multipleTargetIdDecoder: Decoder[TargetIdSet]     =
    Decoder.instance(
      _.get[List[Target.Id]]("ids").map(list => NonEmptySet.of(list.head, list.tail: _*))
    )
  private implicit val targetIdSetDecoder: Decoder[TargetIdSet] =
    singleTargetIdDecoder.or(multipleTargetIdDecoder)

  private implicit val targetWithIdDecoder: Decoder[TargetWithId] = Decoder.instance(c =>
    for {
      id     <- c.as[TargetIdSet]
      target <- c.as[Target]
    } yield (id, target)
  )

  private val obsIdDecoder: Decoder[Observation.Id] = Decoder.instance(_.get[Observation.Id]("id"))

  private implicit val targetEnvIdDecoder: Decoder[TargetEnvId] = Decoder.instance(c =>
    for {
      targetEnvId <- c.get[TargetEnvironment.Id]("id")
      obsId       <- c.get[Option[Observation.Id]]("observation")(decodeOption(obsIdDecoder))
    } yield (targetEnvId, obsId)
  )

  private val singleTargetEnvDecoder: Decoder[TargetEnv] = Decoder.instance(c =>
    for {
      id             <- c.as[TargetEnvId].map(id => NonEmptySet.one(id))
      scienceTargets <- c.get[List[TargetWithId]]("scienceTargets").map(TreeSeqMap.from)
    } yield TargetEnv(id, scienceTargets)
  )

  private val groupTargetEnvDecoder: Decoder[TargetEnv] = Decoder.instance(c =>
    for {
      id             <- c.get[List[TargetEnvId]]("targetEnvironments")
                          .map(list => NonEmptySet.of(list.head, list.tail: _*))
      scienceTargets <- c.get[List[TargetWithId]]("commonTargetList").map(TreeSeqMap.from)
    } yield TargetEnv(id, scienceTargets)
  )

  implicit val decoderTargetEnv: Decoder[TargetEnv] =
    singleTargetEnvDecoder.or(groupTargetEnvDecoder)

  val id: Lens[TargetEnv, TargetEnvIdSet]                              = Focus[TargetEnv](_.id)
  val scienceTargets: Lens[TargetEnv, TreeSeqMap[TargetIdSet, Target]] =
    Focus[TargetEnv](_.scienceTargets)
}
