// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.util.Enumerated

sealed trait ResizableSection extends Product with Serializable {
  def value: String
}

object ResizableSection {
  case object TargetsTree      extends ResizableSection {
    val value = "targets_tree"
  }
  case object ObservationsTree extends ResizableSection {
    val value = "observations_tree"
  }

  /** @group Typeclass Instances */
  implicit val ResizableSectionEnumerated: Enumerated[ResizableSection] =
    Enumerated.of(TargetsTree, ObservationsTree)
}
