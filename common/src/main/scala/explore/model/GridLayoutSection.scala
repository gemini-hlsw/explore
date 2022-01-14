// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.util.Enumerated

sealed trait GridLayoutSection extends Product with Serializable {
  def value: String
}

object GridLayoutSection {
  case object ObservationsLayout extends GridLayoutSection {
    val value = "observations"
  }

  /** @group Typeclass Instances */
  implicit val GridLayoutSectionEnumerated: Enumerated[GridLayoutSection] =
    Enumerated.of(ObservationsLayout)
}
