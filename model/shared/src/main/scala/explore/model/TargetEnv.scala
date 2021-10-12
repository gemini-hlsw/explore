// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import io.circe.Decoder
import lucuma.core.model.Target
import lucuma.core.model.TargetEnvironment
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.TreeSeqMap

case class TargetEnv(
  id:             TargetEnvironment.Id,
  scienceTargets: TreeSeqMap[Target.Id, ScienceTarget]
)

object TargetEnv {
  implicit val eqTargetEnv: Eq[TargetEnv] = Eq.by(x => (x.id, x.scienceTargets.toMap))

  implicit val decoderTargetEnv: Decoder[TargetEnv] = Decoder.instance(c =>
    for {
      id             <- c.downField("id").as[TargetEnvironment.Id]
      scienceTargets <-
        c.downField("scienceTargets")
          .as[List[ScienceTarget]]
          .map(list => TreeSeqMap(list.map(t => t.id -> t): _*))
    } yield TargetEnv(id, scienceTargets)
  )

  val id: Lens[TargetEnv, TargetEnvironment.Id]                             = Focus[TargetEnv](_.id)
  val scienceTargets: Lens[TargetEnv, TreeSeqMap[Target.Id, ScienceTarget]] =
    Focus[TargetEnv](_.scienceTargets)
}
