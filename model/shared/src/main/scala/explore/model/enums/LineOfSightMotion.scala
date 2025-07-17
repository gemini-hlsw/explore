// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

enum LineOfSightMotion(val tag: String) derives Enumerated:
  case CZ extends LineOfSightMotion("CZ")
  case RV extends LineOfSightMotion("RV")
  case Z  extends LineOfSightMotion("Z")
