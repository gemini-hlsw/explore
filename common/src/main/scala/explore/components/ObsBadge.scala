// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.model.ExploreObservation
import explore.model.reusability._
import gem.util.Enumerated
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.elements.icon.Icon
import react.semanticui.views.card._

final case class ObsBadge(obs: ExploreObservation, layout: ObsBadge.Layout)
    extends ReactProps[ObsBadge](ObsBadge.component)
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
          Card(
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
                  case ConfAndConstraints => obs.constraints
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
