// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import crystal.react.View
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.primereact.Dialog

case class About(isOpen: View[Boolean]) extends ReactFnProps(About.component)

object About:
  private type Props = About

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .render((props, ctx) =>
      Dialog(
        visible = props.isOpen.get,
        onHide = props.isOpen.set(false),
        dismissableMask = true,
        clazz = ExploreStyles.Dialog.Small,
        resizable = false,
        header = Logo.component()
      )(
        ExploreCopy(s"Version: ${ctx.version}",
                    ctx.version.value,
                    ExploreStyles.Version,
                    ExploreStyles.VersionUncopied
        )
      )
    )
