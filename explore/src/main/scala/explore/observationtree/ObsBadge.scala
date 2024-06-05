// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.EditableLabel
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.ObsSummary
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.model.Observation
import lucuma.core.util.Enumerated
import lucuma.core.util.Gid
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.InputSwitch
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.TooltipOptions
import lucuma.ui.components.TimeSpanView
import lucuma.ui.primereact.*
import lucuma.ui.primereact.EnumDropdownView
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

case class ObsBadge(
  obs:               ObsSummary,
  executionTime:     Pot[Option[TimeSpan]],
  layout:            ObsBadge.Layout,
  selected:          Boolean = false,
  setStatusCB:       Option[ObsStatus => Callback] = none,
  setActiveStatusCB: Option[ObsActiveStatus => Callback] = none,
  setSubtitleCB:     Option[Option[NonEmptyString] => Callback] = none,
  deleteCB:          Callback,
  cloneCB:           Option[Callback] = none,
  readonly:          Boolean = false
) extends ReactFnProps(ObsBadge.component)

object ObsBadge:
  private type Props = ObsBadge

  enum Section derives Eq:
    case None, Header, Detail

  case class Layout(
    showTitle:         Boolean,
    showSubtitle:      Boolean,
    showConfiguration: Section,
    showConstraints:   Boolean
  ) derives Eq

  object Layout:
    val ObservationsTab: Layout = Layout(true, true, Section.Detail, true)
    val TargetsTab: Layout      = Layout(false, false, Section.Header, true)
    val ConstraintsTab: Layout  = Layout(true, false, Section.Detail, false)

  // TODO Make this a component similar to the one in the docs.
  private def renderEnumProgress[A: Enumerated](value: A): VdomNode = {
    val all = summon[Enumerated[A]].all
    <.progress(^.width := "100%", ^.max := all.length - 1, ^.value := all.indexOf(value))
  }

  private val idIso = Gid[Observation.Id].isoPosLong

  private val component =
    ScalaFnComponent[Props] { props =>
      val obs    = props.obs
      val layout = props.layout

      val deleteButton =
        Button(
          text = true,
          clazz = ExploreStyles.DeleteButton |+| ExploreStyles.ObsDeleteButton,
          icon = Icons.Trash,
          tooltip = "Delete",
          onClickE = e => e.preventDefaultCB *> e.stopPropagationCB *> props.deleteCB
        ).small.unless(props.readonly)

      val duplicateButton =
        Button(
          text = true,
          clazz = ExploreStyles.ObsCloneButton,
          icon = Icons.Clone,
          tooltip = "Duplicate",
          onClickE = e => e.preventDefaultCB *> e.stopPropagationCB *> props.cloneCB.getOrEmpty
        ).small.unless(props.readonly)

      val header =
        <.div(ExploreStyles.ObsBadgeHeader)(
          <.div(ExploreStyles.ObsBadgeTargetAndId)(
            <.div(obs.title).when(layout.showTitle),
            <.div(obs.configurationSummary.getOrElse("-"))
              .when(layout.showConfiguration === Section.Header),
            <.div(
              ExploreStyles.ObsBadgeId,
              s"[${idIso.get(obs.id).value.toHexString}]",
              props.cloneCB.whenDefined(_ => duplicateButton),
              deleteButton
            )
          )
        )

      val meta = <.div(ExploreStyles.ObsBadgeMeta)(
        props.setSubtitleCB
          .map(setCB =>
            EditableLabel(
              value = obs.subtitle,
              mod = setCB,
              editOnClick = false,
              textClass = ExploreStyles.ObsBadgeSubtitle,
              inputClass = ExploreStyles.ObsBadgeSubtitleInput,
              addButtonLabel = "Add description",
              addButtonClass = ExploreStyles.ObsBadgeSubtitleAdd,
              leftButtonClass = ExploreStyles.ObsBadgeSubtitleEdit,
              rightButtonClass = ExploreStyles.ObsBadgeSubtitleDelete,
              readonly = props.readonly
            )
          )
          .whenDefined
          .when(layout.showSubtitle),
        renderEnumProgress(obs.status)
      )

      <.div(
        <.div(ExploreStyles.ObsBadge, ExploreStyles.ObsBadgeSelected.when(props.selected))(
          header,
          meta,
          <.div(ExploreStyles.ObsBadgeDescription)(
            props.setActiveStatusCB.map(_ => ExploreStyles.ObsBadgeHasActiveStatus).orEmpty,
            <.span(
              obs.configurationSummary
                .map(conf => <.div(conf))
                .whenDefined
                .when(layout.showConfiguration === Section.Detail),
              <.div(obs.constraintsSummary).when(layout.showConstraints)
            ),
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
                  tooltipOptions = TooltipOptions(position = Tooltip.Position.Left),
                  disabled = props.readonly
                )
              )(
                // don't select the observation when changing the active status
                ^.onClick ==> { e =>
                  (e.preventDefaultCB >> e.stopPropagationCB).unless_(props.readonly)
                }
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
                  panelClass = ExploreStyles.ObsStatusSelectPanel,
                  disabled = props.readonly
                )
              )(
                // don't select the observation when changing the status
                ^.onClick ==> { e => e.preventDefaultCB >> e.stopPropagationCB }
              )
            ),
            props.executionTime.orSpinner(_.map(TimeSpanView(_)))
          )
        )
      )
    }
