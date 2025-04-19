// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import explore.model.Help
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.Context
import japgolly.scalajs.react.vdom.html_<^.*
import org.http4s.Uri
import org.http4s.implicits.*

case class HelpContext(
  rawUrl:        Uri,
  editUrl:       Uri,
  user:          String,
  project:       String,
  displayedHelp: View[Option[Help.Id]]
)

object HelpContext:
  val ctx: Context[HelpContext] = React.createContext("HelpContext", null) // No default value

  val Provider =
    ScalaFnComponent
      .withHooks[Unit]
      .withPropsChildren
      .useStateView(none[Help.Id]) // displayedHelp
      .render((_, children, displayedHelp) =>
        val helpCtx = HelpContext(
          rawUrl = uri"https://raw.githubusercontent.com",
          editUrl = uri"https://github.com",
          user = "gemini-hlsw",
          project = "explore-help-docs",
          displayedHelp = displayedHelp
        )

        HelpContext.ctx.provide(helpCtx)(children)
      )
