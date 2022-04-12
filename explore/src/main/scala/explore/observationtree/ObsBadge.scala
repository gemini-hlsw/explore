// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.syntax.all._
import crystal.react.View
import crystal.react.reuse._
import eu.timepit.refined.types.string.NonEmptyString
import explore.EditInPlace
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ObsSummary
import explore.model.ObsWithConf
import explore.model.ObsWithConstraints
import explore.model.ObsWithTitle
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.ObsActiveStatus
import lucuma.core.enum.ObsStatus
import lucuma.core.model.Observation
import lucuma.core.util.Enumerated
import lucuma.core.util.Gid
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._
import react.semanticui.collections.form.FormDropdown
import react.semanticui.elements.button.Button
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.dropdown.Dropdown
import react.semanticui.modules.popup.Popup
import react.semanticui.shorthand._
import react.semanticui.sizes._
import react.semanticui.views.card._

final case class ObsBadge(
  obs:               ObsSummary, // The layout will depend on the mixins of the ObsSummary.
  selected:          Boolean = false,
  setStatusCB:       Option[ObsStatus ==> Callback] = none,
  setActiveStatusCB: Option[ObsActiveStatus ==> Callback] = none,
  setSubtitleCB:     Option[Option[NonEmptyString] ==> Callback] = none,
  deleteCB:          Option[Reuse[Callback]] = none
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
            clazz = ExploreStyles.DeleteButton |+| ExploreStyles.ObsDeleteButton,
            icon = Icons.Trash,
            onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
              e.preventDefaultCB *>
                e.stopPropagationCB *>
                props.deleteCB.map(_.value: Callback).getOrEmpty
          )

        def titleAndId(title: Option[NonEmptyString]) =
          <.div(
            ExploreStyles.ObsBadgeTargetAndId,
            title.map(<.div(_)).whenDefined,
            <.div(ExploreStyles.ObsBadgeId, s"[${idIso.get(obs.id).value.toHexString}]")
          )

        <.div(
          Card(raised = props.selected)(ExploreStyles.ObsBadge)(
            CardContent(
              CardHeader(
                <.div(
                  ExploreStyles.ObsBadgeHeader,
                  obs match {
                    case withTitle: ObsWithTitle => titleAndId(withTitle.title.some)
                    case withConf: ObsWithConf   => withConf.conf
                    case _                       => titleAndId(none)
                  },
                  props.deleteCB.whenDefined(_ => deleteButton)
                )
              ),
              CardMeta(
                obs match {
                  case withTitle: ObsWithTitle =>
                    props.setSubtitleCB
                      .map(setCB =>
                        EditInPlace(
                          withTitle.subtitle,
                          setCB,
                          ExploreStyles.ObsBadgeSubtitle,
                          ("Add description": VdomNode).reuseAlways,
                          ExploreStyles.ObsBadgeSubtitleAdd,
                          ExploreStyles.BlendedButton |+| ExploreStyles.ObsBadgeSubtitleEdit,
                          ExploreStyles.BlendedButton |+| ExploreStyles.ObsBadgeSubtitleDelete
                        )
                      )
                      .whenDefined
                },
                renderEnumProgress(obs.status)
              ),
              CardDescription()(ExploreStyles.ObsBadgeDescription)(
                obs match {
                  case _: ObsWithTitle                     =>
                    ReactFragment(List(conf, constraints).flatten: _*)
                  case withConstraints: ObsWithConstraints =>
                    ReactFragment(withConstraints.constraintsSummary)
                  case _                                   =>
                    ReactFragment(obs.id.toString)
                },
                props.setActiveStatusCB.map(setActiveStatus =>
                  Popup(
                    clazz = ExploreStyles.Compact,
                    content = obs.activeStatus match {
                      case ObsActiveStatus.Active   => "Observation is active"
                      case ObsActiveStatus.Inactive => "Observation is not active"
                    },
                    trigger = Checkbox(
                      toggle = true,
                      checked = obs.activeStatus.toBoolean,
                      onClickE = (e: ReactEvent, _: Checkbox.CheckboxProps) =>
                        e.preventDefaultCB >> e.stopPropagationCB >> setActiveStatus(
                          ObsActiveStatus.FromBoolean.get(!obs.activeStatus.toBoolean)
                        ),
                      clazz = ExploreStyles.ObsActiveStatusToggle
                    )
                  )
                )
              ),
              CardExtra(clazz = ExploreStyles.ObsBadgeExtra)(
                props.setStatusCB.map(setStatus =>
                  EnumViewSelect(
                    id = s"obs-status-${obs.id}-2",
                    value = View[ObsStatus](
                      obs.status,
                      { (f, cb) =>
                        val newValue = f(obs.status)
                        setStatus(newValue) >> cb(newValue)
                      }
                    ),
                    compact = true,
                    onClickE = (e: ReactEvent, _: Dropdown.DropdownProps) =>
                      e.preventDefaultCB >> e.stopPropagationCB,
                    onChangeE = (e: ReactEvent, _: FormDropdown.FormDropdownProps) =>
                      e.preventDefaultCB >> e.stopPropagationCB,
                    clazz = ExploreStyles.ObsStatusSelect
                  )
                ),
                <.span(
                  s"${obs.duration.toHours}hrs ${obs.duration.toMinutes % 60}mins"
                )
              )
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
