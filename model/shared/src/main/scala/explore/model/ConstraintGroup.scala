// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import io.circe.generic.semiauto.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.schemas.decoders.given
import monocle.Focus

import scala.annotation.unused

case class ConstraintGroup(constraintSet: ConstraintSet, obsIds: ObsIdSet) derives Eq {
  def addObsId(obsId: Observation.Id): ConstraintGroup =
    ConstraintGroup.obsIds.modify(_.add(obsId))(this)

  def removeObsIds(toRemove: ObsIdSet): Option[ConstraintGroup] =
    obsIds.remove(toRemove).map(ids => ConstraintGroup.obsIds.replace(ids)(this))

  def addObsIds(toAdd: ObsIdSet): ConstraintGroup =
    ConstraintGroup.obsIds.modify(_ ++ toAdd)(this)

  def asKeyValue: (ObsIdSet, ConstraintGroup) = (this.obsIds, this)
}

object ConstraintGroup {
  val constraintSet = Focus[ConstraintGroup](_.constraintSet)
  val obsIds        = Focus[ConstraintGroup](_.obsIds)

  private case class ObsMatch(id: Observation.Id)
  @unused("used but compiler can't figure it out")
  private implicit val obsMatchDecoder: Decoder[ObsMatch] = deriveDecoder

  private case class ObsIdMatches(matches: List[ObsMatch])
  private implicit val obsIdMatchesDecoder: Decoder[ObsIdMatches] = deriveDecoder

  implicit val constraintGroupDecoder: Decoder[ConstraintGroup] =
    Decoder.instance(c =>
      for {
        cs     <- c.downField("constraintSet").as[ConstraintSet]
        obsIds <- c.downField("observations").as[ObsIdMatches].map { o =>
                    val ids = o.matches.map(_.id)
                    ObsIdSet.of(ids.head, ids.tail: _*)
                  }
      } yield ConstraintGroup(cs, obsIds)
    )

}
