// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

enum GridBreakpointName(val tag: String) derives Enumerated:
  case XXS extends GridBreakpointName("xxs")
  case XS  extends GridBreakpointName("xs")
  case MD  extends GridBreakpointName("md")
  case LG  extends GridBreakpointName("lg")
  case SM  extends GridBreakpointName("sm")
  case XL  extends GridBreakpointName("xl")
  case XXL extends GridBreakpointName("xxl")
