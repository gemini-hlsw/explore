// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import crystal.react.reuse.Reuse
import crystal.react.View

package object graphql {
  type ReuseView[A] = Reuse[View[A]]
}
