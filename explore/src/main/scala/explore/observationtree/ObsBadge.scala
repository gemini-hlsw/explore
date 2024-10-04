// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Eq
import cats.data.NonEmptySet
import cats.derived.*
import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.EditableLabel
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.Observation
import explore.model.display.given
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.enums.ScienceBand
import lucuma.core.syntax.all.*
import lucuma.core.util.Enumerated
import lucuma.core.util.Gid
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.LayeredIcon
import lucuma.react.fa.TextLayer
import lucuma.react.primereact.Button
import lucuma.react.primereact.InputSwitch
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.TooltipOptions
import lucuma.react.primereact.hooks.all.*
import lucuma.ui.components.TimeSpanView
import lucuma.ui.primereact.*
import lucuma.ui.primereact.EnumDropdownView
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

import scala.collection.immutable.SortedSet

case class ObsBadge(
  obs:                   Observation,
  executionTime:         Pot[Option[TimeSpan]],
  layout:                ObsBadge.Layout,
  selected:              Boolean = false,
  setStatusCB:           Option[ObsStatus => Callback] = none,
  setActiveStatusCB:     Option[ObsActiveStatus => Callback] = none,
  setSubtitleCB:         Option[Option[NonEmptyString] => Callback] = none,
  setScienceBandCB:      Option[ScienceBand => Callback] = none,
  deleteCB:              Callback,
  cloneCB:               Option[Callback] = none,
  allocatedScienceBands: SortedSet[ScienceBand],
  readonly:              Boolean = false
) extends ReactFnProps(ObsBadge.component):
  val isDisabled: Boolean      = readonly || obs.isCalibration
  val nonEmptyAllocatedBands   = NonEmptySet.fromSet(allocatedScienceBands)
  val scienceBandIsInvalid     = obs.scienceBand.exists(b => !allocatedScienceBands.contains(b))
  val showScienceBand: Boolean =
    obs.calibrationRole.isEmpty && (allocatedScienceBands.size > 1 || scienceBandIsInvalid)

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
    ScalaFnComponent
      .withHooks[Props]
      .usePopupMenuRef
      .render { (props, menuRef) =>
        val obs    = props.obs
        val layout = props.layout

        val deleteButton =
          Button(
            text = true,
            clazz = ExploreStyles.DeleteButton |+| ExploreStyles.ObsDeleteButton,
            icon = Icons.Trash,
            tooltip = "Delete",
            onClickE = e => e.preventDefaultCB *> e.stopPropagationCB *> props.deleteCB
          ).small.unless(props.isDisabled)

        val duplicateButton =
          Button(
            text = true,
            clazz = ExploreStyles.ObsCloneButton,
            icon = Icons.Clone,
            tooltip = "Duplicate",
            onClickE = e => e.preventDefaultCB *> e.stopPropagationCB *> props.cloneCB.getOrEmpty
          ).small.unless(props.isDisabled)

        val scienceBandIcon =
          LayeredIcon(fixedWidth = true)(
            Icons.Circle,
            TextLayer(obs.scienceBand.map(b => (b.ordinal + 1).toString).getOrElse("-"),
                      inverse = false
            )
          )

        val scienceBandToolTip: String =
          val action =
            if (obs.scienceBand.isEmpty || props.scienceBandIsInvalid) "set" else "change"
          List(
            obs.scienceBand.map(_.longName).getOrElse("Science band not set").some,
            props.setScienceBandCB.map(_ => s"Click to $action")
          ).flatten
            .mkString("\n")

        val scienceBandButton =
          Button(
            text = true,
            clazz = ExploreStyles.ObsScienceBandButton,
            icon = scienceBandIcon,
            tooltip = scienceBandToolTip,
            onClickE = e =>
              // don't show menu if there is no callback defined
              e.preventDefaultCB *> e.stopPropagationCB *>
                menuRef.toggle(e).when(props.setScienceBandCB.isDefined).void
          )

        val header =
          <.div(ExploreStyles.ObsBadgeHeader)(
            <.div(ExploreStyles.ObsBadgeTargetAndId)(
              <.div(obs.title).when(layout.showTitle),
              <.div(obs.configurationSummary.getOrElse("-"))
                .when(layout.showConfiguration === Section.Header),
              <.div(
                ExploreStyles.ObsBadgeId,
                scienceBandButton.when(props.showScienceBand),
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
                readonly = props.isDisabled
              )
            )
            .whenDefined
            .when(layout.showSubtitle),
          renderEnumProgress(obs.status)
        )

        val validationTooltip = <.div(
          obs.validations
            .toTagMod(ov => <.div(ov.code.name, <.ul(ov.messages.toList.toTagMod(<.li(_)))))
        )
        val validationIcon    = Tooltip.Fragment(content = validationTooltip)(<.span(Icons.ErrorIcon))

        React.Fragment(
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
                        setActiveStatus(
                          ObsActiveStatus.FromBoolean.get(!obs.activeStatus.toBoolean)
                        ),
                      clazz = ExploreStyles.ObsActiveStatusToggle,
                      tooltip = obs.activeStatus match
                        case ObsActiveStatus.Active   => "Observation is active"
                        case ObsActiveStatus.Inactive => "Observation is not active"
                      ,
                      tooltipOptions = TooltipOptions(position = Tooltip.Position.Left),
                      disabled = props.isDisabled
                    )
                  )(
                    // don't select the observation when changing the active status
                    ^.onClick ==> { e =>
                      (e.preventDefaultCB >> e.stopPropagationCB).unless_(props.isDisabled)
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
                        (f, cb) =>
                          val oldValue = obs.status
                          val newValue = f(obs.status)
                          setStatus(newValue) >> cb(oldValue, newValue)
                      ),
                      size = PlSize.Mini,
                      clazz = ExploreStyles.ObsStatusSelect,
                      panelClass = ExploreStyles.ObsStatusSelectPanel,
                      disabled = props.isDisabled
                    )
                  )(
                    // don't select the observation when changing the status
                    ^.onClick ==> { e => e.preventDefaultCB >> e.stopPropagationCB }
                  )
                ),
                props.executionTime.orSpinner(_.map(TimeSpanView(_))),
                validationIcon.unless(obs.validations.isEmpty)
              )
            )
          ),
          (props.nonEmptyAllocatedBands, props.setScienceBandCB).mapN: (bs, cb) =>
            ScienceBandPopupMenu(
              currentBand = obs.scienceBand,
              allocatedScienceBands = bs,
              onSelect = cb,
              menuRef = menuRef
            )
        )
      }
