// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.fa

import japgolly.scalajs.react._
import japgolly.scalajs.react.facade.JsNumber
import japgolly.scalajs.react.vdom.html_<^._
import react.common._

import scala.annotation.nowarn
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation._
import scala.scalajs.js.|

/**
 * Facade for the react component for FontAwesomeIcons See:
 * https://fontawesome.com/v5.15/how-to-use/on-the-web/using-with/react
 */
final case class FontAwesomeIcon(
  family:                 Family,
  icon:                   js.UndefOr[react.fa.FontAwesomeIcon.Icon],
  clazz:                  js.UndefOr[Css],
  color:                  js.UndefOr[String],
  spin:                   js.UndefOr[Boolean],
  pulse:                  js.UndefOr[Boolean],
  border:                 js.UndefOr[Boolean],
  fixedWidth:             js.UndefOr[Boolean],
  inverse:                js.UndefOr[Boolean],
  listItem:               js.UndefOr[Boolean],
  flip:                   js.UndefOr[Flip],
  size:                   js.UndefOr[IconSize],
  pull:                   js.UndefOr[Pull],
  rotation:               js.UndefOr[Rotation],
  transform:              js.UndefOr[String | Transform],
  tabIndex:               js.UndefOr[Int],
  style:                  js.UndefOr[Style],
  title:                  js.UndefOr[String],
  swapOpacity:            js.UndefOr[Boolean],
  override val modifiers: Seq[TagMod]
) extends GenericFnComponentPA[FontAwesomeIcon.Props, FontAwesomeIcon] {
  override protected def cprops    = FontAwesomeIcon.props(this)
  override protected val component = FontAwesomeIcon.component
  override def addModifiers(modifiers: Seq[TagMod]) = copy(modifiers = this.modifiers ++ modifiers)

  def size(size: IconSize): FontAwesomeIcon = copy(size = size)

  def title(title: String): FontAwesomeIcon = copy(title = title)

  def color(color: String): FontAwesomeIcon = copy(color = color)

  def clazz(clazz: Css): FontAwesomeIcon = copy(clazz = clazz)

  def inverse(inverse: Boolean = true): FontAwesomeIcon = copy(inverse = inverse)

  def transform(transform: Transform): FontAwesomeIcon = copy(transform = transform)

  def spin(spin: Boolean = false): FontAwesomeIcon = copy(spin = spin)

  def fixedWidth(fixedWidth: Boolean = true): FontAwesomeIcon = copy(fixedWidth = fixedWidth)
}

@js.native
@nowarn
trait FAIcon extends js.Object {
  val iconName: String = js.native
  val prefix: String   = js.native
}

@js.native
trait Transform extends js.Object {
  var size: js.UndefOr[JsNumber]
  var x: js.UndefOr[JsNumber]
  var y: js.UndefOr[JsNumber]
  var rotate: js.UndefOr[JsNumber]
  var flipX: js.UndefOr[Boolean]
  var flipY: js.UndefOr[Boolean]
}

object Transform {
  def apply(
    size:   js.UndefOr[JsNumber] = js.undefined,
    x:      js.UndefOr[JsNumber] = js.undefined,
    y:      js.UndefOr[JsNumber] = js.undefined,
    rotate: js.UndefOr[JsNumber] = js.undefined,
    flipX:  js.UndefOr[Boolean] = js.undefined,
    flipY:  js.UndefOr[Boolean] = js.undefined
  ): Transform = {
    val p = (new js.Object).asInstanceOf[Transform]
    size.foreach(v => p.size = v)
    x.foreach(v => p.x = v)
    y.foreach(v => p.y = v)
    rotate.foreach(v => p.rotate = v)
    flipX.foreach(v => p.flipX = v)
    flipY.foreach(v => p.flipY = v)
    p
  }
}

@js.native
@JSImport("@fortawesome/fontawesome-svg-core", "library")
object IconLibrary extends js.Object {
  @nowarn
  def add(arg: FAIcon*): js.Any = js.native
}

object FontAwesomeIcon {
  type Icon            = String | List[String]
  private type RawIcon = String | js.Array[String]

  def apply(
    faIcon:      FAIcon,
    clazz:       js.UndefOr[Css] = js.undefined,
    color:       js.UndefOr[String] = js.undefined,
    spin:        js.UndefOr[Boolean] = js.undefined,
    pulse:       js.UndefOr[Boolean] = js.undefined,
    border:      js.UndefOr[Boolean] = js.undefined,
    fixedWidth:  js.UndefOr[Boolean] = js.undefined,
    inverse:     js.UndefOr[Boolean] = js.undefined,
    listItem:    js.UndefOr[Boolean] = js.undefined,
    flip:        js.UndefOr[Flip] = js.undefined,
    size:        js.UndefOr[IconSize] = js.undefined,
    pull:        js.UndefOr[Pull] = js.undefined,
    rotation:    js.UndefOr[Rotation] = js.undefined,
    transform:   js.UndefOr[String | Transform] = js.undefined,
    tabIndex:    js.UndefOr[Int] = js.undefined,
    style:       js.UndefOr[Style] = js.undefined,
    title:       js.UndefOr[String] = js.undefined,
    swapOpacity: js.UndefOr[Boolean] = js.undefined,
    modifiers:   Seq[TagMod] = Seq.empty
  ): FontAwesomeIcon = new FontAwesomeIcon(
    Family.fromString(faIcon.prefix),
    faIcon.iconName,
    clazz,
    color,
    spin,
    pulse,
    border,
    fixedWidth,
    inverse,
    listItem,
    flip,
    size,
    pull,
    rotation,
    transform,
    tabIndex,
    style,
    title,
    swapOpacity,
    modifiers
  )

  def layered(icons: FontAwesomeIcon*): TagMod = {
    val attrs: Seq[TagMod] = (^.cls := "fa-layers fa-fw") +: icons.map(x => x: TagMod)
    <.span(attrs: _*)
  }

  @js.native
  @JSImport("@fortawesome/react-fontawesome", "FontAwesomeIcon")
  private object FontAwesomeIcon extends js.Function1[js.Any, js.Any] {
    def apply(arg: js.Any): js.Any = js.native
  }

  @js.native
  trait Props extends js.Object {
    var icon: js.UndefOr[RawIcon]
    // mask?: IconProp
    var className: js.UndefOr[String]
    var color: js.UndefOr[String]
    var spin: js.UndefOr[Boolean]
    var pulse: js.UndefOr[Boolean]
    var border: js.UndefOr[Boolean]
    var fixedWidth: js.UndefOr[Boolean]
    var inverse: js.UndefOr[Boolean]
    var listItem: js.UndefOr[Boolean]
    var flip: js.UndefOr[String]
    var size: js.UndefOr[String]
    var pull: js.UndefOr[String]
    var rotation: js.UndefOr[Int]
    var transform: js.UndefOr[String | Transform]
    // symbol?: FaSymbol
    var style: js.UndefOr[js.Object]
    var tabIndex: js.UndefOr[Int]
    var title: js.UndefOr[String]
    var swapOpacity: js.UndefOr[Boolean];
  }

  protected def props(p: FontAwesomeIcon): Props =
    rawprops(
      p.family,
      p.icon,
      p.clazz,
      p.color,
      p.spin,
      p.pulse,
      p.border,
      p.fixedWidth,
      p.inverse,
      p.listItem,
      p.flip,
      p.size,
      p.pull,
      p.rotation,
      p.transform,
      p.tabIndex,
      p.style,
      p.title,
      p.swapOpacity
    )

  protected def rawprops(
    family:      Family,
    icon:        js.UndefOr[Icon],
    clazz:       js.UndefOr[Css],
    color:       js.UndefOr[String],
    spin:        js.UndefOr[Boolean],
    pulse:       js.UndefOr[Boolean],
    border:      js.UndefOr[Boolean],
    fixedWidth:  js.UndefOr[Boolean],
    inverse:     js.UndefOr[Boolean],
    listItem:    js.UndefOr[Boolean],
    flip:        js.UndefOr[Flip],
    size:        js.UndefOr[IconSize],
    pull:        js.UndefOr[Pull],
    rotation:    js.UndefOr[Rotation],
    transform:   js.UndefOr[String | Transform],
    tabIndex:    js.UndefOr[Int],
    style:       js.UndefOr[Style],
    title:       js.UndefOr[String],
    swapOpacity: js.UndefOr[Boolean]
  ): Props = {
    val p = (new js.Object).asInstanceOf[Props]
    icon.foreach { d =>
      p.icon = (d: Any) match {
        case s: String => List(family.prefix, s).toJSArray
        case s         => s.asInstanceOf[List[String]].toJSArray
      }
    }
    clazz.foreach(v => p.className = v.htmlClass)
    color.foreach(v => p.color = v)
    spin.foreach(v => p.spin = v)
    pulse.foreach(v => p.pulse = v)
    border.foreach(v => p.border = v)
    fixedWidth.foreach(v => p.fixedWidth = v)
    inverse.foreach(v => p.inverse = v)
    listItem.foreach(v => p.listItem = v)
    flip.foreach(v => p.flip = v.toJs)
    size.foreach(v => p.size = v.toJs)
    pull.foreach(v => p.pull = v.toJs)
    rotation.foreach(v =>
      p.rotation = v match {
        case Rotation.Rotate90  => 90
        case Rotation.Rotate180 => 180
        case Rotation.Rotate270 => 270
      }
    )
    transform.foreach(v => p.transform = v)
    tabIndex.foreach(v => p.tabIndex = v)
    style.foreach(v => p.style = v.toJsObject)
    title.foreach(v => p.title = v)
    swapOpacity.foreach(v => p.swapOpacity = v)
    p
  }

  private val component = JsFnComponent[Props, Children.None](FontAwesomeIcon)
}
