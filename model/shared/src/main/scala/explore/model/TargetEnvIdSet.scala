// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order
import cats.data.NonEmptySet
import explore.model.util.NonEmptySetWrapper
import lucuma.core.model.TargetEnvironment
import monocle.Iso

final case class TargetEnvIdSet(idSet: NonEmptySet[TargetEnvironment.Id])

object TargetEnvIdSet {
  implicit val orderTargetEnvGroupIdSet: Order[TargetEnvIdSet] = Order.by(_.idSet)

  val iso: Iso[TargetEnvIdSet, NonEmptySet[TargetEnvironment.Id]] =
    Iso[TargetEnvIdSet, NonEmptySet[TargetEnvironment.Id]](_.idSet)(TargetEnvIdSet.apply)

  implicit def nonEmptySetWrapper(ids: TargetEnvIdSet) = NonEmptySetWrapper(ids, iso)
}
