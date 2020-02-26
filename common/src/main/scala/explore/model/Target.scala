// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import japgolly.scalajs.react.Reusability

sealed trait Target
object Target {
  final case object M81 extends Target
  final case object M51 extends Target

  implicit val reuse: Reusability[Target] = Reusability.derive
}
