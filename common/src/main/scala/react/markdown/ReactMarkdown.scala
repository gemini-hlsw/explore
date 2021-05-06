// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.markdown

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom._
import react.common._
import sttp.model.Uri

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation._

sealed trait RemarkPlugin extends js.Object

object RemarkPlugin {

  val RemarkGFM =
    if (scala.scalajs.runtime.linkingInfo.productionMode) RemarkGFMProd else RemarkGFMDev

  @js.native
  @JSImport("remark-gfm", JSImport.Namespace)
  object RemarkGFMDev extends js.Object with RemarkPlugin

  @js.native
  @JSImport("remark-gfm", JSImport.Default)
  object RemarkGFMProd extends js.Object with RemarkPlugin

  val RemarkMath =
    if (scala.scalajs.runtime.linkingInfo.productionMode) RemarkMathProd else RemarkMathDev

  @js.native
  @JSImport("remark-math", JSImport.Namespace)
  object RemarkMathDev extends js.Object with RemarkPlugin

  @js.native
  @JSImport("remark-math", JSImport.Default)
  object RemarkMathProd extends js.Object with RemarkPlugin

}

sealed trait RehypePlugin extends js.Object

object RehypePlugin {
  val RehypeKatex =
    if (scala.scalajs.runtime.linkingInfo.productionMode) RehypeKatexProd else RehypeKatexDev

  @js.native
  @JSImport("rehype-katex", JSImport.Namespace)
  object RehypeKatexDev extends js.Object with RehypePlugin

  @js.native
  @JSImport("rehype-katex", JSImport.Default)
  object RehypeKatexProd extends js.Object with RehypePlugin
}

final case class ReactMarkdown(
  content:                js.UndefOr[String],
  clazz:                  js.UndefOr[Css] = js.undefined,
  linkTarget:             js.UndefOr[String] = js.undefined,
  transformImageUri:      js.UndefOr[Uri => Uri] = js.undefined,
  remarkPlugins:          js.UndefOr[List[RemarkPlugin]] = js.undefined,
  rehypePlugins:          js.UndefOr[List[RehypePlugin]] = js.undefined,
  override val modifiers: Seq[TagMod] = Seq.empty
) extends GenericFnComponentPA[ReactMarkdown.Props, ReactMarkdown] {
  override protected def cprops    = ReactMarkdown.props(this)
  override protected val component = ReactMarkdown.component
  override def addModifiers(modifiers: Seq[TagMod]) = copy(modifiers = this.modifiers ++ modifiers)
}

object ReactMarkdown {

  val ReactMarkdown =
    if (scala.scalajs.runtime.linkingInfo.productionMode) ReactMarkdownProd else ReactMarkdownDev

  @js.native
  @JSImport("react-markdown", JSImport.Default)
  object ReactMarkdownProd extends js.Function1[js.Any, js.Any] {
    def apply(arg: js.Any): js.Any = js.native
  }

  @js.native
  @JSImport("react-markdown", JSImport.Namespace)
  object ReactMarkdownDev extends js.Function1[js.Any, js.Any] {
    def apply(arg: js.Any): js.Any = js.native
  }

  @js.native
  trait Props extends js.Object {
    var children: js.UndefOr[String]
    var className: js.UndefOr[String]
    var linkTarget: js.UndefOr[String]
    var transformImageUri: js.UndefOr[js.Function1[String, String]]
    var remarkPlugins: js.UndefOr[js.Array[js.Object]]
    var rehypePlugins: js.UndefOr[js.Array[js.Object]]
  }

  protected def props(p: ReactMarkdown): Props =
    rawprops(p.content,
             p.clazz,
             p.linkTarget,
             p.transformImageUri,
             p.remarkPlugins,
             p.rehypePlugins
    )

  protected def rawprops(
    content:           js.UndefOr[String],
    clazz:             js.UndefOr[Css],
    linkTarget:        js.UndefOr[String],
    transformImageUri: js.UndefOr[Uri => Uri],
    remarkPlugins:     js.UndefOr[List[RemarkPlugin]],
    rehypePlugins:     js.UndefOr[List[RehypePlugin]]
  ): Props = {
    val p = (new js.Object).asInstanceOf[Props]
    content.foreach(v => p.children = v)
    clazz.foreach(v => p.className = v.htmlClass)
    linkTarget.foreach(v => p.linkTarget = v)
    transformImageUri.foreach(v =>
      p.transformImageUri =
        ((u: String) => v(Uri.unsafeParse(u)).toString()): js.Function1[String, String]
    )
    remarkPlugins.foreach(l => p.remarkPlugins = l.map(_.asInstanceOf[js.Object]).toJSArray)
    rehypePlugins.foreach(l => p.rehypePlugins = l.map(_.asInstanceOf[js.Object]).toJSArray)
    p
  }

  private val component = JsFnComponent[Props, Children.None](ReactMarkdown)
}
