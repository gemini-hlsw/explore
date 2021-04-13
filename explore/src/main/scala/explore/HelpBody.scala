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
import monocle.macros.Lenses
import react.common._
import react.markdown.ReactMarkdown
import react.semanticui._
import react.semanticui.elements.button.Button
import react.semanticui.sizes._
import sttp.client3._
import sttp.client3.impl.cats.FetchCatsBackend
import sttp.model.Uri

import scala.concurrent.duration._
import scala.util.Try

final case class HelpBody(base: HelpContext, helpId: Help.Id)(implicit val ctx: AppContextIO)
    extends ReactProps[HelpBody](HelpBody.component) {
  private val path        = Uri.relative(helpId.value.split("/").toList)
  private val rootUrl     = base.rawUrl.addPath(List(base.user.value, base.project.value))
  private val baseUrl     =
    base.rawUrl.addPath(List(base.user.value, base.project.value, "main") ++ path.path.init)
  private val url         = rootUrl.addPath("main" :: path.path.toList)
  private val rootEditUrl = base.editUrl.addPath(List(base.user.value, base.project.value))
  private val newPage     = rootEditUrl
    .addPath(List("new", "main") ++ (path.path.init :+ "/"))
    .addParam("filename", path.path.last)
    .addParam("value", s"# Title")
    .addParam("message", s"Create $helpId")
  private val editPage    = rootEditUrl
    .addPath(List("edit", "main") ++ path.path)
    .addParam("message", s"Update $helpId")
}

object HelpBody {
  type Props = HelpBody

  @Lenses
  final case class State(content: Pot[String])

  def load(uri: Uri): IO[Try[String]] = {
    val backend = FetchCatsBackend[IO]()
    basicRequest
      .get(uri)
      .readTimeout(5.seconds)
      .send(backend)
      .map {
        _.body.leftMap(s => new RuntimeException(s)).toTry
      }
  }

  private val component =
    ScalaComponent
      .builder[Props]
      .initialState(State(Pot.pending))
      .render_PS { (p, s) =>
        val imageConv = (s: Uri) => p.baseUrl.addPath(s.path)
        AppCtx.using { implicit ctx =>
          HelpCtx.usingView { helpCtx =>
            val helpView = helpCtx.zoom(HelpContext.displayedHelp)
            val editUrl  = s.content match {
              case Ready(_) => p.editPage
              case _        => p.newPage
            }
            <.div(
              ExploreStyles.HelpSidebar,
              <.div(
                ExploreStyles.HelpTitle,
                <.h4(ExploreStyles.HelpTitleLabel, "Help"),
                <.div(
                  Button(as = <.a,
                         size = Mini,
                         compact = true,
                         onClick = helpView.set(None).runAsyncCB
                  )(
                    Icons.Edit.size(Small).fitted(true)
                  )(^.href := editUrl.toString(), ^.target := "_blank"),
                  Button(size = Mini, compact = true, onClick = helpView.set(None).runAsyncCB)(
                    Icons.Close.size(Small).fitted(true)
                  )
                )
              ),
              <.div(
                ExploreStyles.HelpBody,
                <.div(
                  ExploreStyles.HelpBody,
                  s.content match {
                    case Ready(a)                                         =>
                      ReactMarkdown(content = a,
                                    clazz = ExploreStyles.HelpMarkdownBody,
                                    linkTarget = "_blank",
                                    imageConv
                      ): VdomNode
                    case Pending(_)                                       => <.div(ExploreStyles.HelpMarkdownBody, "Loading...")
                    case crystal.Error(o) if o.getMessage.contains("405") =>
                      <.div(
                        ExploreStyles.HelpMarkdownBody,
                        "Not found, maybe you want to create it ",
                        <.a(^.href := p.newPage.toString(),
                            ^.target := "_blank",
                            Icons.Edit.link(true)
                        )
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
      }
      .componentDidMount { $ =>
        implicit val ctx = $.props.ctx
        load($.props.url)
          .flatMap(v => $.modStateIn[IO](State.content.set(Pot.fromTry(v))))
          .runAsyncAndForgetCB
      }
      .build

}
