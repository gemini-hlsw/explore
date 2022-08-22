// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

enum ResizableSection(val value: String):
  case TargetsTree        extends ResizableSection("targets_tree")
  case ObservationsTree   extends ResizableSection("observations_tree")
  case ConstraintSetsTree extends ResizableSection("constraintsets_tree")
