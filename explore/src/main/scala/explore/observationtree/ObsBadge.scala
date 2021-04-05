// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.ObsSummary
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.util.Enumerated
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._
import react.semanticui.elements.button.Button
import react.semanticui.sizes._
import react.semanticui.views.card._
import lucuma.core.util.Gid

final case class ObsBadge(
  obs:      ObsSummary,
  layout:   ObsBadge.Layout,
  selected: Boolean = false,
  deleteCB: Option[Observation.Id => IO[Unit]] = None
) extends ReactProps[ObsBadge](ObsBadge.component)

object ObsBadge {
  type Props = ObsBadge

  sealed trait Layout
  object Layout {
    final case object NameAndConf               extends Layout
    final case object ConfAndConstraints        extends Layout
    final case object NameAndConfAndConstraints extends Layout

    implicit val layoutReuse: Reusability[Layout] = Reusability.derive
  }

  protected implicit val propsReuse: Reusability[Props] = Reusability.caseClassExcept("deleteCB")

  // TODO Make this a component similar to the one in the docs.
  def renderEnumProgress[A: Enumerated](value: A): VdomNode = {
    val all = implicitly[Enumerated[A]].all
    <.progress(^.width := "100%", ^.max := all.length - 1, ^.value := all.indexOf(value))
  }

  import Layout._
  val idIso = Gid[Observation.Id].isoPosLong

  protected val component =
    ScalaComponent
      .builder[Props]
      .render_P { props =>
        val obs          = props.obs
        val deleteButton = Button(
          size = Small,
          compact = true,
          clazz = ExploreStyles.DeleteButton |+| ExploreStyles.ObservationDeleteButton,
          onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
            e.preventDefaultCB *> e.stopPropagationCB *> props.deleteCB
              .map(cb => cb(props.obs.id).runAsyncAndForgetCB)
              .getOrEmpty
        )(
          Icons.Trash
        )

        <.small(
          Card(raised = props.selected)(ExploreStyles.ObsBadge)(
            CardContent(
              CardHeader(
                <.div(
                  ExploreStyles.ObservationCardHeader,
                  props.layout match {
                    case NameAndConf | NameAndConfAndConstraints =>
                      val targetName: String = obs.pointingName match {
                        case Some(Right(n)) => n.value
                        case Some(Left(n))  => n.map(_.value).getOrElse("<Asterism>")
                        case None           => "<No target>"
                      }
                      <.div(
                        ExploreStyles.ObsBadgeTargetAndId,
                        <.div(targetName),
                        <.div(ExploreStyles.ObsBadgeId, s"[${idIso.get(obs.id).value.toHexString}]")
                      )
                    case ConfAndConstraints                      => obs.conf
                  },
                  props.deleteCB.whenDefined(_ => deleteButton)
                )
              ),
              CardMeta(
                renderEnumProgress(obs.status)
              ),
              CardDescription(
                props.layout match {
                  case NameAndConf               => ReactFragment(obs.conf)
                  case ConfAndConstraints        => ReactFragment(obs.constraintsSummary)
                  case NameAndConfAndConstraints =>
                    ReactFragment(<.div(obs.conf), <.div(obs.constraintsSummary))
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
