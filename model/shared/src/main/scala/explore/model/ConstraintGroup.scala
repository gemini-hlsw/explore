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

case class ConstraintGroup(constraintSet: ConstraintSet, obsIds: ObsIdSet) derives Eq

object ConstraintGroup:
  val constraintSet = Focus[ConstraintGroup](_.constraintSet)
  val obsIds        = Focus[ConstraintGroup](_.obsIds)

  def fromTuple(tuple: (ObsIdSet, ConstraintSet)): ConstraintGroup =
    ConstraintGroup(tuple._2, tuple._1)
