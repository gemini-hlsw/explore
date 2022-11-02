// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import react.common.ReactFnProps
import explore.common.TimingQueries.*
// import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import crystal.react.View
import react.primereact.*
import lucuma.ui.primereact.*
import explore.Icons
import explore.components.ui.ExploreStyles

case class TimingWindowsPanel(windows: View[TimingWindowResult])
    extends ReactFnProps(TimingWindowsPanel.component)

object TimingWindowsPanel:
  private type Props = TimingWindowsPanel

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .render { props =>
        <.div(
          ExploreStyles.TimingWindowsBody,
          <.ul(
            props.windows.get.tmpTimingWindows
              .map { w =>
                w.id
              }
              .mkTagMod(<.li)
          ),
          <.div(
            ExploreStyles.TimingWindowEditor
          ),
          Button(size = Button.Size.Small).compact.small(Icons.ThinPlus)
        )
      }
