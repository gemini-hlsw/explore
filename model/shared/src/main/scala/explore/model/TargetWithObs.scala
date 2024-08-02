// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.implicits.*
import io.circe.Decoder
import io.circe.Decoder.*
import io.circe.generic.semiauto.*
import lucuma.core.model.Target
import lucuma.schemas.decoders.given
import lucuma.schemas.model.*
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedSet

// We keep track of the observations a target is in to know if it's shared or not.
case class TargetWithObs(target: Target, obsIds: SortedSet[Observation.Id]) derives Eq {
  def addObsIds(ids: ObsIdSet): TargetWithObs =
    TargetWithObs.obsIds.modify(_ ++ ids.toSortedSet)(this)

  def removeObsIds(ids: ObsIdSet): TargetWithObs =
    TargetWithObs.obsIds.modify(_ -- ids.toSortedSet)(this)
}

object TargetWithObs {
  val target: Lens[TargetWithObs, Target] =
    Focus[TargetWithObs](_.target)

  val obsIds: Lens[TargetWithObs, SortedSet[Observation.Id]] =
    Focus[TargetWithObs](_.obsIds)
}

case class TargetWithIdAndObs(id: Target.Id, targetWithObs: TargetWithObs) derives Eq {
  def target: Target                    = targetWithObs.target
  def obsIds: SortedSet[Observation.Id] = targetWithObs.obsIds

  def toTargetWithId: TargetWithId = TargetWithId(id, targetWithObs.target)

  def addObsIds(ids: ObsIdSet): TargetWithIdAndObs =
    TargetWithIdAndObs.targetWithObs.modify(_.addObsIds(ids))(this)

  def removeObsIds(ids: ObsIdSet): TargetWithIdAndObs =
    TargetWithIdAndObs.targetWithObs.modify(_.removeObsIds(ids))(this)
}

object TargetWithIdAndObs {
  val id: Lens[TargetWithIdAndObs, Target.Id] =
    Focus[TargetWithIdAndObs](_.id)

  val targetWithObs: Lens[TargetWithIdAndObs, TargetWithObs] =
    Focus[TargetWithIdAndObs](_.targetWithObs)

  private case class ObsIdMatch(id: Observation.Id)
  private given Decoder[ObsIdMatch] = deriveDecoder

  private case class ObsIdMatches(matches: List[ObsIdMatch])
  private given Decoder[ObsIdMatches] = deriveDecoder

  given Decoder[TargetWithIdAndObs] = Decoder.instance(c =>
    for {
      targetWithId <- c.get[TargetWithId]("target")
      obsIds       <- c.downField("observations").as[ObsIdMatches].map(_.matches.map(_.id))
    } yield TargetWithIdAndObs(
      targetWithId.id,
      TargetWithObs(targetWithId.target, SortedSet.from(obsIds))
    )
  )
}
