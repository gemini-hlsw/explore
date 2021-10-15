// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptySet
import explore.model.decoders._
import io.circe.Decoder
import lucuma.core.model.Target
import lucuma.core.model.TargetEnvironment
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.TreeSeqMap

case class TargetEnv(
  id:             TargetEnvironment.Id,
  scienceTargets: TreeSeqMap[TargetIdSet, Target]
)

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

  implicit val decoderTargetEnv: Decoder[TargetEnv] = Decoder.instance(c =>
    for {
      id             <- c.get[TargetEnvironment.Id]("id")
      scienceTargets <- c.get[List[TargetWithId]]("scienceTargets").map(TreeSeqMap.from)
    } yield TargetEnv(id, scienceTargets)
  )

  val id: Lens[TargetEnv, TargetEnvironment.Id]                        = Focus[TargetEnv](_.id)
  val scienceTargets: Lens[TargetEnv, TreeSeqMap[TargetIdSet, Target]] =
    Focus[TargetEnv](_.scienceTargets)
}
