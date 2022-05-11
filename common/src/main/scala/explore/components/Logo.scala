// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object Logo {
  def apply() = ScalaComponent.static(
    <.div(ExploreStyles.LoginTitleWrapper)(
      <.div(ExploreStyles.LoginTitle, "Explore")
    )
  )()
}
