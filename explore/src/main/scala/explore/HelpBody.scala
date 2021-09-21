// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect._
import cats.syntax.all._
import crystal.Pending
import crystal.Pot
import crystal.Ready
import crystal.react.implicits._
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.Help
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Focus
import org.http4s._
import org.http4s.dom.FetchClientBuilder
import react.common._
import react.hotkeys._
import react.markdown.ReactMarkdown
import react.markdown.RehypePlugin
import react.markdown.RemarkPlugin
import react.semanticui._
import react.semanticui.elements.button.Button
import react.semanticui.sizes._

import scala.concurrent.duration._
import scala.util.Try

final case class HelpBody(base: HelpContext, helpId: Help.Id)(implicit val ctx: AppContextIO)
    extends ReactProps[HelpBody](HelpBody.component) {
  private val path        = Uri.Path.unsafeFromString(helpId.value)
  private val rootUrl     = base.rawUrl / base.user.value / base.project.value
  private val baseUrl     =
    path.segments.init.foldLeft(base.rawUrl / base.user.value / base.project.value / "main")(
      (uri, segment) => uri / segment.encoded
    )
  private val url         = rootUrl / "main" / path
  private val rootEditUrl = base.editUrl / base.user.value / base.project.value
  private val newPage     = (rootEditUrl / "new" / "main")
    .withQueryParam("filename", path.segments.mkString("/"))
    .withQueryParam("value", s"# Title")
    .withQueryParam("message", s"Create $helpId")
  private val editPage    = (rootEditUrl / "edit" / "main" / path)
    .withQueryParam("message", s"Update $helpId")
}

// This is a sort of facade to get dynamic loading boundaries right
// You'd be tempted to put this inside HelpBody but it will make it load HelpBody as part of the main bundle
class HelpLoader {
  def loadHelp(helpCtx: HelpContext, h: Help.Id)(implicit ctx: AppContextIO): VdomElement =
    HelpBody(helpCtx, h)
}

object HelpBody {
  type Props = HelpBody

  final case class State(content: Pot[String])

  object State {
    val content = Focus[State](_.content)
  }

  def load(uri: Uri): IO[Try[String]] =
    FetchClientBuilder[IO]
      .withRequestTimeout(5.seconds)
      .create
      .get(uri)(r => r.attemptAs[String].value)
      .map(_.toTry)
      .handleError { case x =>
        scala.util.Failure(x)
      }

  private val component =
    ScalaComponent
      .builder[Props]
      .initialState(State(Pot.pending))
      .render_PS { (p, s) =>
        val imageConv = (s: Uri) => p.baseUrl / s.path

        HelpCtx.usingView { helpCtx =>
          val helpView = helpCtx.zoom(HelpContext.displayedHelp)
          val editUrl  = s.content match {
            case Ready(_) => p.editPage
            case _        => p.newPage
          }
          <.div(
            ExploreStyles.HelpSidebar,
            GlobalHotKeys(keyMap = KeyMap("CLOSE_HELP" -> "ESC"),
                          handlers = Handlers("CLOSE_HELP" -> helpView.set(none).toCB)
            ),
            <.div(
              ExploreStyles.HelpTitle,
              <.h4(ExploreStyles.HelpTitleLabel, "Help"),
              <.div(
                Button(as = <.a, size = Mini, compact = true, onClick = helpView.set(None))(
                  Icons.Edit
                )(^.href := editUrl.toString(), ^.target := "_blank"),
                Button(size = Mini, compact = true, onClick = helpView.set(None))(
                  Icons.Close
                )
              )
            ),
            <.div(
              ExploreStyles.HelpBody,
              <.div(
                ExploreStyles.HelpBody,
                s.content match {
                  case Ready(a)                                         =>
                    ReactMarkdown(
                      content = a,
                      clazz = ExploreStyles.HelpMarkdownBody,
                      linkTarget = "_blank",
                      imageConv,
                      remarkPlugins = List(RemarkPlugin.RemarkMath, RemarkPlugin.RemarkGFM),
                      rehypePlugins = List(RehypePlugin.RehypeKatex)
                    ): VdomNode
                  case Pending(_)                                       => <.div(ExploreStyles.HelpMarkdownBody, "Loading...")
                  case crystal.Error(o) if o.getMessage.contains("404") =>
                    <.div(
                      ExploreStyles.HelpMarkdownBody,
                      "Not found, maybe you want to create it ",
                      <.a(^.href := p.newPage.toString(), ^.target := "_blank", Icons.Edit)
                    )
                  case crystal.Error(_)                                 =>
                    <.div(
                      ExploreStyles.HelpMarkdownBody,
                      "We encountered an error trying to read the help file"
                    )
                }
              )
            )
          )
        }
      }
      .componentDidMount { $ =>
        implicit val ctx = $.props.ctx

        load($.props.url)
          .flatMap(v => $.modStateIn[IO](State.content.replace(Pot.fromTry(v))))
          .runAsync
      }
      .build

}
