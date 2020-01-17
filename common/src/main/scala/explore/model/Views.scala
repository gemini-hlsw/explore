// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.effect._
import crystal._

object Views {
  import AppState.rootModel

  lazy val target: View[IO, Target] = rootModel.view(RootModel.target)
}
