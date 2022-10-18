// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

/**
 * Enum to give an id to each table
 */
enum TableId(val tag: String) derives Enumerated:
  case ConstraintsSummary extends TableId("constraints_summary")
