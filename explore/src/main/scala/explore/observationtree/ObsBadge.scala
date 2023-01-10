// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.syntax.all.*
import crystal.react.View
import crystal.react.reuse.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.EditableLabel
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.ObsSummary
import explore.model.ObsWithConf
import explore.model.ObsWithConstraints
import explore.model.ObsWithTitle
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.model.Observation
import lucuma.core.util.Enumerated
import lucuma.core.util.Gid
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.primereact.EnumDropdownView
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.floatingui.Placement
import react.floatingui.syntax.*
import react.primereact.Button
import react.primereact.InputSwitch
import react.primereact.TooltipOptions

case class ObsBadge(
  obs:               ObsSummary, // The layout will depend on the mixins of the ObsSummary.
  selected:          Boolean = false,
  setStatusCB:       Option[ObsStatus => Callback] = none,
  setActiveStatusCB: Option[ObsActiveStatus => Callback] = none,
  setSubtitleCB:     Option[Option[NonEmptyString] => Callback] = none,
  deleteCB:          Option[Callback] = none,
  cloneCB:           Option[Callback] = none
) extends ReactFnProps(ObsBadge.component)

object ObsBadge {
  private type Props = ObsBadge

  // TODO Make this a component similar to the one in the docs.
  private def renderEnumProgress[A: Enumerated](value: A): VdomNode = {
    val all = summon[Enumerated[A]].all
    <.progress(^.width := "100%", ^.max := all.length - 1, ^.value := all.indexOf(value))
  }

  private val idIso = Gid[Observation.Id].isoPosLong

  private val component =
    ScalaFnComponent[Props] { props =>
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
          text = true,
          clazz = ExploreStyles.DeleteButton |+| ExploreStyles.ObsDeleteButton,
          icon = Icons.Trash,
          tooltip = "Delete",
          onClickE = e => e.preventDefaultCB *> e.stopPropagationCB *> props.deleteCB.getOrEmpty
        ).small

      val duplicateButton =
        Button(
          text = true,
          clazz = ExploreStyles.ObsCloneButton,
          icon = Icons.Clone,
          tooltip = "Duplicate",
          onClickE = e => e.preventDefaultCB *> e.stopPropagationCB *> props.cloneCB.getOrEmpty
        ).small

      def titleAndId(title: String) =
        <.div(
          ExploreStyles.ObsBadgeTargetAndId,
          <.div(title),
          <.div(ExploreStyles.ObsBadgeId, s"[${idIso.get(obs.id).value.toHexString}]")
        )

      val header = <.div(
        ExploreStyles.ObsBadgeHeader,
        obs match {
          case withTitle: ObsWithTitle => titleAndId(withTitle.title)
          case withConf: ObsWithConf   => titleAndId(withConf.conf)
          case _                       => titleAndId("")
        },
        props.cloneCB.whenDefined(_ => duplicateButton),
        props.deleteCB.whenDefined(_ => deleteButton)
      )

      val meta = <.div(
        ExploreStyles.ObsBadgeMeta,
        obs match {
          case withTitle: ObsWithTitle =>
            props.setSubtitleCB
              .map(setCB =>
                EditableLabel(
                  value = withTitle.subtitle,
                  mod = setCB,
                  editOnClick = false,
                  textClass = ExploreStyles.ObsBadgeSubtitle,
                  inputClass = ExploreStyles.ObsBadgeSubtitleInput,
                  addButtonLabel = "Add description",
                  addButtonClass = ExploreStyles.ObsBadgeSubtitleAdd,
                  leftButtonClass =
                    ExploreStyles.BlendedButton |+| ExploreStyles.ObsBadgeSubtitleEdit,
                  rightButtonClass =
                    ExploreStyles.BlendedButton |+| ExploreStyles.ObsBadgeSubtitleDelete
                )
              )
              .whenDefined
          case _                       => TagMod.empty
        },
        renderEnumProgress(obs.status)
      )

      <.div(
        <.div(ExploreStyles.ObsBadge, ExploreStyles.ObsBadgeSelected.when(props.selected))(
          header,
          meta,
          <.div(ExploreStyles.ObsBadgeDescription)(
            props.setActiveStatusCB.map(_ => ExploreStyles.ObsBadgeHasActiveStatus).orEmpty,
            obs match {
              case _: ObsWithTitle                     =>
                <.span(List(conf, constraints).flatten: _*)
              case withConstraints: ObsWithConstraints =>
                <.span(withConstraints.constraintsSummary)
              case _                                   =>
                <.span(obs.id.toString)
            },
            props.setActiveStatusCB.map(setActiveStatus =>
              <.span(
                InputSwitch(
                  checked = obs.activeStatus.toBoolean,
                  onChange = _ =>
                    setActiveStatus(ObsActiveStatus.FromBoolean.get(!obs.activeStatus.toBoolean)),
                  clazz = ExploreStyles.ObsActiveStatusToggle,
                  tooltip = obs.activeStatus match
                    case ObsActiveStatus.Active   => "Observation is active"
                    case ObsActiveStatus.Inactive => "Observation is not active"
                  ,
                  tooltipOptions = TooltipOptions(position = TooltipOptions.Position.Left)
                )
              )(
                // don't select the observation when changing the active status
                ^.onClick ==> { e => e.preventDefaultCB >> e.stopPropagationCB }
              )
            )
          ),
          <.div(ExploreStyles.ObsBadgeExtra)(
            props.setStatusCB.map(setStatus =>
              <.span(
                ExploreStyles.ObsStatusSelectWrapper,
                EnumDropdownView(
                  id = NonEmptyString.unsafeFrom(s"obs-status-${obs.id}-2"),
                  value = View[ObsStatus](
                    obs.status,
                    { (f, cb) =>
                      val newValue = f(obs.status)
                      setStatus(newValue) >> cb(newValue)
                    }
                  ),
                  size = PlSize.Mini,
                  clazz = ExploreStyles.ObsStatusSelect,
                  panelClass = ExploreStyles.ObsStatusSelectPanel
                )
              )(
                // don't select the observation when changing the status
                ^.onClick ==> { e => e.preventDefaultCB >> e.stopPropagationCB }
              )
            ),
            <.span(
              s"${obs.duration.toHours}hrs ${obs.duration.toMinutes % 60}mins"
            )
          )
        )
      )
    }
}
