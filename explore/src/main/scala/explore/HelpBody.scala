// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.*
import cats.syntax.all.*
import crystal.Pot
import crystal.react.hooks.*
import crystal.react.implicits.*
import explore.components.ui.ExploreStyles
import explore.model.Help
import explore.syntax.ui.*
import explore.syntax.ui.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.http4s.*
import org.http4s.dom.FetchClientBuilder
import react.common.ReactFnProps
import react.hotkeys.*
import react.hotkeys.hooks.*
import react.markdown.ReactMarkdown
import react.markdown.RehypePlugin
import react.markdown.RemarkPlugin
import react.semanticui.elements.button.Button
import react.semanticui.sizes.*

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
        load(props.url).flatMap(v => state.set(Pot.fromTry(v)).to[IO])
      }
      .useHotkeysBy((_, helpCtx, _) => UseHotkeysProps("esc", helpCtx.displayedHelp.set(none)))
      .render { (props, helpCtx, state, _) =>
        val imageConv = (s: Uri) => props.baseUrl.addPath(s.path)

        val helpView = helpCtx.displayedHelp
        val editUrl  = state.get match {
          case Pot.Ready(_) => props.editPage
          case _            => props.newPage
        }

        <.div(
          ExploreStyles.HelpSidebar,
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
            state.get match {
              case Pot.Ready(a)                                 =>
                ReactMarkdown(
                  content = a,
                  clazz = ExploreStyles.HelpMarkdownBody,
                  linkTarget = "_blank",
                  imageConv,
                  remarkPlugins = List(RemarkPlugin.RemarkMath, RemarkPlugin.RemarkGFM),
                  rehypePlugins = List(RehypePlugin.RehypeKatex)
                ): VdomNode
              case Pot.Pending                                  => <.div(ExploreStyles.HelpMarkdownBody, "Loading...")
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
