// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Show
import explore.model.enum._

object show {
  implicit val showProposalClass: Show[ProposalClass] =
    Show.show(_.label)

  implicit val showTacCategory: Show[TacCategory] =
    Show.show(_.label)

  implicit val showToOActivation: Show[ToOActivation] =
    Show.show(_.label)
}
