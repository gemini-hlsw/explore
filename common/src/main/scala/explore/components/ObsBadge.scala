// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.components.ui.GPPStyles
import explore.model.reusability._
import gem.util.Enumerated
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.views.card._
import explore.model.ObsSummary

final case class ObsBadge(
  obs:      ObsSummary,
  layout:   ObsBadge.Layout,
  selected: Boolean = false
) extends ReactProps[ObsBadge](ObsBadge.component)
object ObsBadge {
  type Props = ObsBadge

  sealed trait Layout
  object Layout {
    final case object NameAndConf        extends Layout
    final case object ConfAndConstraints extends Layout
  }

  protected implicit val layoutReuse: Reusability[Layout] = Reusability.derive
  protected implicit val propsReuse: Reusability[Props]   = Reusability.derive

  // TODO Make this a component similar to the one in the docs.
  def renderEnumProgress[A: Enumerated](value: A): VdomNode = {
    val all = implicitly[Enumerated[A]].all
    <.progress(^.width := "100%", ^.max := all.length - 1, ^.value := all.indexOf(value))
  }

  import Layout._

  protected val component =
    ScalaComponent
      .builder[Props]
      .render_P { props =>
        val obs = props.obs

        <.small(
          Card(raised = props.selected)(GPPStyles.ObsBadge)(
            CardContent(
              CardHeader(
                props.layout match {
                  case NameAndConf        => obs.target.name
                  case ConfAndConstraints => obs.conf
                }
              ),
              CardMeta(
                renderEnumProgress(obs.status)
              ),
              CardDescription(
                props.layout match {
                  case NameAndConf        => obs.conf
                  case ConfAndConstraints => obs.constraints.name
                }
              ),
              CardExtra(
                // select new status
                s"${obs.duration.toHours}hrs ${obs.duration.toMinutes % 60}mins"
              )
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
