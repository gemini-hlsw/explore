// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all.*
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*
import lucuma.react.markdown.ReactMarkdown
import lucuma.react.markdown.RehypePlugin
import lucuma.react.markdown.RemarkPlugin
import lucuma.react.primereact.*
import lucuma.react.primereact.tooltip.*
import lucuma.ui.optics.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given

case class MarkdownEditor(
  value:       View[Option[NonEmptyString]],
  readonly:    Boolean = false,
  placeholder: String = "Use Edit tab to edit note. Markdown is supported."
) extends ReactFnProps(MarkdownEditor)

object MarkdownEditor
    extends ReactFnComponent[MarkdownEditor](props =>
      for {
        id <- useId
      } yield {
        val placeholder    = if (props.readonly) "" else props.placeholder
        val markdownViewer = ReactMarkdown(
          content = props.value.get.map(_.value).getOrElse(placeholder),
          clazz = ExploreStyles.HelpMarkdownBody |+|
            ExploreStyles.MarkdownPlaceholder.when_(props.value.get.isEmpty),
          remarkPlugins = List(RemarkPlugin.RemarkMath, RemarkPlugin.RemarkGFM),
          rehypePlugins = List(RehypePlugin.RehypeExternalLinks, RehypePlugin.RehypeKatex)
        )

        <.div(
          ExploreStyles.MarkdownEditor,
          markdownViewer.when(props.readonly),
          TabView(
            clazz = ExploreStyles.FullHeightTabView,
            panels = List(
              TabPanel(header = "View")(markdownViewer),
              TabPanel(header =
                <.span("Edit").withTooltip(content = "Edit note. Markdown is supported.")
              )(
                FormInputTextAreaView(
                  id = NonEmptyString.unsafeFrom(id),
                  value = props.value.as(OptionNonEmptyStringIso)
                )
              )
            )
          ).unless(props.readonly)
        )
      }
    )
