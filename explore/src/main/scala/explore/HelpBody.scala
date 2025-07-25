// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import crystal.syntax.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Help
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.markdown.ReactMarkdown
import lucuma.react.markdown.RehypePlugin
import lucuma.react.markdown.RemarkPlugin
import lucuma.react.primereact.Button
import lucuma.ui.primereact.*
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import org.http4s.*
import org.http4s.client.Client

import scala.util.Try

case class HelpBody(base: HelpContext, helpId: Help.Id, userVault: Option[UserVault])
    extends ReactFnProps(HelpBody.component):
  private val path: Uri.Path = Uri.Path.unsafeFromString(helpId.value)
  private val rootUrl: Uri   = base.rawUrl / base.user / base.project
  private val baseUrl: Uri   =
    path.segments.init.foldLeft(base.rawUrl / base.user / base.project / "main")((uri, segment) =>
      uri / segment.encoded
    )
  private val mainUrl        = rootUrl / "main"
  private val url            = mainUrl.addPath(path)
  private val rootEditUrl    = base.editUrl / base.user / base.project
  private val newPage        = (rootEditUrl / "new" / "main")
    .withQueryParam("filename", path.segments.mkString("/"))
    .withQueryParam("value", s"# Title")
    .withQueryParam("message", s"Create $helpId")
  private val editPage       = (rootEditUrl / "edit" / "main")
    .addPath(path)
    .withQueryParam("message", s"Update $helpId")

object HelpBody:
  private type Props = HelpBody

  private def load(uri: Uri, client: Client[IO]): IO[Try[String]] =
    client
      .get(uri)(r => r.attemptAs[String].value)
      .attempt
      .map(_.flatten.toTry)

  val themeAttr = VdomAttr("data-theme")

  private val component = ScalaFnComponent[Props]: props =>
    for
      ctx     <- useContext(AppContext.ctx)
      helpCtx <- useContext(HelpContext.ctx)
      state   <- useStateView(pending[String])
      _       <- useEffectOnMount:
                   load(props.url, ctx.httpClient).flatMap(v => state.set(Pot.fromTry(v)).toAsync)
    yield
      val imageConv = (s: Uri) => s.host.fold(props.baseUrl.addPath(s.path))(_ => s)

      val helpView = helpCtx.displayedHelp
      val editUrl  = state.get match {
        case Pot.Ready(_) => props.editPage
        case _            => props.newPage
      }

      React.Fragment(
        <.div(ExploreStyles.HelpTitle)(
          <.h4("Help"),
          <.div(
            TagMod.when(props.userVault.isStaff)(
              <.a(
                Button(
                  icon = Icons.Edit,
                  severity = Button.Severity.Secondary,
                  onClick = helpView.set(None)
                ).mini.compact,
                ^.href   := editUrl.toString(),
                ^.target := "_blank"
              )
            ),
            Button(
              icon = Icons.Close,
              severity = Button.Severity.Secondary,
              onClick = helpView.set(None)
            ).mini.compact
          )
        ),
        <.div(
          ExploreStyles.HelpBody,
          themeAttr := "light",
          state.get match {
            case Pot.Ready(a)                                 =>
              ReactMarkdown(
                content = a,
                clazz = ExploreStyles.HelpMarkdownBody,
                imageConv,
                remarkPlugins = List(RemarkPlugin.RemarkMath, RemarkPlugin.RemarkGFM),
                rehypePlugins = List(RehypePlugin.RehypeExternalLinks, RehypePlugin.RehypeKatex)
              )
            case Pot.Pending                                  =>
              <.div(ExploreStyles.HelpMarkdownBody, "Loading...")
            case Pot.Error(o) if o.getMessage.contains("404") =>
              <.div(ExploreStyles.HelpMarkdownBody)(
                "Not found",
                TagMod.when(props.userVault.isStaff)(
                  React.Fragment(
                    ", maybe you want to create it ",
                    <.a(^.href := props.newPage.toString(), ^.target := "_blank", Icons.Edit)
                  )
                )
              )
            case Pot.Error(_)                                 =>
              <.div(
                ExploreStyles.HelpMarkdownBody,
                "We encountered an error trying to read the help file"
              )
          }
        )
      )
