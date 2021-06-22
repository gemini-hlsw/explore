// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.SyncIO
import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse._
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.ObsSummary
import explore.model.ObsWithConf
import explore.model.ObsWithConstraints
import explore.model.ObsWithPointing
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.util.Enumerated
import lucuma.core.util.Gid
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._
import react.semanticui.elements.button.Button
import react.semanticui.sizes._
import react.semanticui.views.card._

final case class ObsBadge(
  obs:      ObsSummary, // The layout will depend on the mixins of the ObsSummary.
  selected: Boolean = false,
  deleteCB: Option[Observation.Id ==> SyncIO[Unit]] = None
) extends ReactProps[ObsBadge](ObsBadge.component)

object ObsBadge {
  type Props = ObsBadge

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  // TODO Make this a component similar to the one in the docs.
  private def renderEnumProgress[A: Enumerated](value: A): VdomNode = {
    val all = implicitly[Enumerated[A]].all
    <.progress(^.width := "100%", ^.max := all.length - 1, ^.value := all.indexOf(value))
  }

  private val idIso = Gid[Observation.Id].isoPosLong

  protected val component =
    ScalaComponent
      .builder[Props]
      .render_P { props =>
        val obs         = props.obs
        val conf        = obs match {
          case withConf: ObsWithConf => (withConf.conf: VdomNode).some
          case _                     => none
        }
        val constraints = obs match {
          case withConstraints: ObsWithConstraints =>
            (withConstraints.constraintsSummary: VdomNode).some
          case _                                   => none
        }

        val deleteButton =
          Button(
            size = Small,
            compact = true,
            clazz = ExploreStyles.DeleteButton |+| ExploreStyles.ObservationDeleteButton,
            icon = true,
            onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
              e.preventDefaultCB *> e.stopPropagationCB *> props.deleteCB
                .map(cb => cb.value(props.obs.id).toCB)
                .getOrEmpty
          )(
            Icons.Trash
          )

        def nameAndId(name: String) =
          <.div(
            ExploreStyles.ObsBadgeTargetAndId,
            <.div(name),
            <.div(ExploreStyles.ObsBadgeId, s"[${idIso.get(obs.id).value.toHexString}]")
          )

        <.small(
          Card(raised = props.selected)(ExploreStyles.ObsBadge)(
            CardContent(
              CardHeader(
                <.div(
                  ExploreStyles.ObservationCardHeader,
                  obs match {
                    case withPointing: ObsWithPointing =>
                      nameAndId(withPointing.pointingName.toString)
                    case withConf: ObsWithConf         => withConf.conf
                    case _                             => nameAndId("")
                  },
                  props.deleteCB.whenDefined(_ => deleteButton)
                )
              ),
              CardMeta(
                renderEnumProgress(obs.status)
              ),
              CardDescription(
                obs match {
                  case _: ObsWithPointing                  =>
                    ReactFragment(List(conf, constraints).flatten: _*)
                  case withConstraints: ObsWithConstraints =>
                    ReactFragment(withConstraints.constraintsSummary)
                  case _                                   =>
                    ReactFragment(obs.id.toString)
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
