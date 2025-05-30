// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.NewBoolean

/** Visibility of an item. */
object Visible extends NewBoolean { inline def Shown = True; inline def Hidden = False }
type Visible = Visible.Type
