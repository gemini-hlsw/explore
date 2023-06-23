// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.given

object Logo {

  val component = ScalaComponent.static(
    <.div(ExploreStyles.LoginTitleWrapper)(
      <.div(ExploreStyles.LoginTitle, "Explore")
    )
  )
}
