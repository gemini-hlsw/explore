// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import org.scalacheck.Cogen

object all
    extends ArbConstraints
    with ArbSiderealTarget
    with ArbExploreSiderealTarget
    with ArbFocused
    with ArbRootModel
    with CogenUUID
