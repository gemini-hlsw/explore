// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.implicits._

final case class SideButton(title: String)

object SideButton {
  implicit val eqSideButton: Eq[SideButton] = Eq.by(_.title)
}
