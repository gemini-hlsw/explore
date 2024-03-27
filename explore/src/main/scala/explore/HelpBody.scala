// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import explore.components.ui.ExploreStyles
import explore.model.Help
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.markdown.ReactMarkdown
import lucuma.react.markdown.RehypePlugin
import lucuma.react.markdown.RemarkPlugin
import lucuma.react.primereact.Button
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import org.http4s.*
import org.http4s.dom.FetchClientBuilder

import scala.concurrent.duration.*
import scala.util.Try

case class HelpBody(base: HelpContext, helpId: Help.Id) extends ReactFnProps(HelpBody.component):
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

  private def load(uri: Uri): IO[Try[String]] =
    FetchClientBuilder[IO]
      .withRequestTimeout(5.seconds)
      .create
      .get(uri)(r => r.attemptAs[String].value)
      .map(_.toTry)
      .handleError { case x =>
        scala.util.Failure(x)
      }

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(HelpContext.ctx)
      .useStateView(Pot.pending[String])
      .useEffectOnMountBy { (props, _, state) =>
        load(props.url).flatMap(v => state.set(Pot.fromTry(v)).toAsync)
      }
      .render { (props, helpCtx, state) =>
        val imageConv = (s: Uri) => props.baseUrl.addPath(s.path)

        val helpView = helpCtx.displayedHelp
        val editUrl  = state.get match {
          case Pot.Ready(_) => props.editPage
          case _            => props.newPage
        }

        React.Fragment(
          <.div(
            ExploreStyles.HelpTitle,
            <.h4("Help"),
            <.div(
              <.a(Button(icon = Icons.Edit,
                         severity = Button.Severity.Secondary,
                         onClick = helpView.set(None)
                  ).mini.compact,
                  ^.href   := editUrl.toString(),
                  ^.target := "_blank"
              ),
              Button(icon = Icons.Close,
                     severity = Button.Severity.Secondary,
                     onClick = helpView.set(None)
              ).mini.compact
            )
          ),
          <.div(
            ExploreStyles.HelpBody,
            state.get match {
              case Pot.Ready(a)                                 =>
                ReactMarkdown(
                  content = a,
                  clazz = ExploreStyles.HelpMarkdownBody,
                  imageConv,
                  remarkPlugins = List(RemarkPlugin.RemarkMath, RemarkPlugin.RemarkGFM),
                  rehypePlugins = List(RehypePlugin.RehypeKatex)
                )
              case Pot.Pending                                  =>
                <.div(ExploreStyles.HelpMarkdownBody, "Loading...")
              case Pot.Error(o) if o.getMessage.contains("404") =>
                <.div(
                  ExploreStyles.HelpMarkdownBody,
                  "Not found, maybe you want to create it ",
                  <.a(^.href := props.newPage.toString(), ^.target := "_blank", Icons.Edit)
                )
              case Pot.Error(_)                                 =>
                <.div(
                  ExploreStyles.HelpMarkdownBody,
                  "We encountered an error trying to read the help file"
                )
            }
          )
        )
      }
