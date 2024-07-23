// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.derived.*
import cats.effect.IO
import cats.kernel.Eq
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.GroupQueries
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.GroupTree
import explore.model.ProgramTimeRange
import explore.syntax.ui.*
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.react.*
import lucuma.react.common.*
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.GroupPropertiesInput
import lucuma.schemas.ObservationDB.Types.TimeSpanInput
import lucuma.ui.components.TimeSpanView
import lucuma.ui.format.TimeSpanFormatter.HoursMinutesAbbreviation
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import monocle.Iso
import monocle.Lens

import scala.scalajs.js

case class GroupEditTile(
  group:             UndoSetter[GroupTree.Group],
  elementsLength:    Int,
  timeEstimateRange: Pot[Option[ProgramTimeRange]],
  renderInTitle:     Tile.RenderInTitle
) extends ReactFnProps(GroupEditTile.component)

object GroupEditTile:
  private type Props = GroupEditTile

  private enum GroupEditType(val tag: String) derives Enumerated, Eq:
    case And extends GroupEditType("And")
    case Or  extends GroupEditType("Or")

  private given Display[GroupEditType] = Display.byShortName(_.tag)

  private def minimumForGroup(t: GroupEditType): Option[NonNegShort] =
    if t === GroupEditType.And then none
    else NonNegShort.from(1).toOption

  val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    // editType
    .useStateViewBy((props, _) =>
      if props.group.get.isAnd then GroupEditType.And else GroupEditType.Or
    )
    // isLoading
    .useStateView(false)
    // nameDisplay
    .useStateBy((props, _, _, _) => props.group.get.name)
    .render: (props, ctx, editType, isLoading, nameDisplay) =>
      import ctx.given

      val group = props.group.get
      val isAnd = group.isAnd

      def groupModView[A](lens: Lens[GroupTree.Group, A], prop: A => GroupPropertiesInput) =
        props.group
          .undoableView(lens)
          .withOnMod(a =>
            GroupQueries.updateGroup[IO](group.id, prop(a)).switching(isLoading.async).runAsync
          )

      val minRequiredV = groupModView(
        GroupTree.Group.minimumRequired,
        m => GroupPropertiesInput(minimumRequired = m.orUnassign)
      )
      val nameV        = groupModView(
        GroupTree.Group.name,
        n => GroupPropertiesInput(name = n.orUnassign)
      )
      val orderedV     =
        groupModView(GroupTree.Group.ordered, o => GroupPropertiesInput(ordered = o.assign))

      val timeSpanOrEmptyLens =
        Iso[Option[TimeSpan], TimeSpan](_.orEmpty)(_.some.filterNot(_.isZero))
      val minIntervalV        = groupModView(
        GroupTree.Group.minimumInterval.andThen(timeSpanOrEmptyLens),
        ts =>
          GroupPropertiesInput(minimumInterval =
            TimeSpanInput(microseconds = ts.toMicroseconds.assign).assign
          )
      )
      val maxIntervalV        = groupModView(
        GroupTree.Group.maximumInterval.andThen(timeSpanOrEmptyLens),
        ts =>
          GroupPropertiesInput(maximumInterval =
            TimeSpanInput(microseconds = ts.toMicroseconds.assign).assign
          )
      )

      val changeGroupTypeButtons = <.div(
        ExploreStyles.GroupChangeButtons,
        SelectButtonEnumView(
          "groupType".refined,
          editType,
          label = "Type",
          disabled = isLoading.get,
          itemTemplate = _.value match
            case GroupEditType.And => <.span("AND (Scheduling)")
            case GroupEditType.Or  => <.span("OR (Choose ", <.i("n"), ")")
          ,
          onChange = tp => minRequiredV.set(minimumForGroup(tp))
        )
      )

      val note = <.div(
        ExploreStyles.GroupEditNote,
        Icons.Note,
        if isAnd then
          <.div(
            "The scheduler will respect any absolute timing constraints each individual observation may have, but also apply any relative timing constraints between them as specified here."
          )
        else
          <.div(
            "The scheduler will select only ",
            <.i("n"),
            " of the observations in this group, respecting any timing constraints the individual observations may have. Once they have been started, no others in the group will be considered for future scheduling."
          )
      )

      val selectGroupForm = <.div(
        ExploreStyles.GroupTypeSelect,
        changeGroupTypeButtons,
        note
      )

      val nameForm = <.div(
        FormInputText(
          id = "nameInput".refined,
          label = "Name",
          value = nameDisplay.value.fold(js.undefined)(_.value),
          disabled = isLoading.get,
          onChange = e => nameDisplay.setState(NonEmptyString.from(e.target.value).toOption),
          onBlur = e => nameV.set(NonEmptyString.from(e.target.value).toOption)
        )
      )

      val minRequiredForm = <.div(
        "Observe ",
        InputNumber(
          "minRequiredInput",
          placeholder = "1",
          value = minRequiredV.get.fold(js.undefined)(_.value),
          disabled = isLoading.get,
          min = 0,
          max = props.elementsLength,
          size = minRequiredV.get.fold(js.undefined)(_.toString.length),
          onValueChange = e =>
            val newMin = e.valueOption
              .flatMap(s => NonNegShort.from(s.toShort).toOption)
            minRequiredV.set(newMin)
        ),
        s" of ${props.elementsLength} observations"
      )

      val orderForm = <.div(
        CheckboxView(
          id = "orderedCheck".refined,
          value = orderedV,
          label = "Ordered",
          disabled = isLoading.get
        )
      )

      val delaysForm = <.div(
        ExploreStyles.GroupDelaysForm,
        FormTimeSpanInput(value = minIntervalV,
                          id = "minDelay".refined,
                          label = "Minimum delay",
                          min = TimeSpan.Zero,
                          max = maxIntervalV.get,
                          disabled = isLoading.get
        ),
        FormTimeSpanInput(value = maxIntervalV,
                          id = "maxDelay".refined,
                          label = "Maximum delay",
                          min = minIntervalV.get,
                          disabled = isLoading.get
        )
      )

      val plannedTime =
        props.timeEstimateRange.orSpinner(_.map: timeEstimateRange =>
          <.div(ExploreStyles.GroupPlannedTime)(
            if timeEstimateRange.maximum === timeEstimateRange.minimum then
              React.Fragment(
                FormLabel(htmlFor = "plannedTime".refined)("Planned Time"),
                TimeSpanView(timeEstimateRange.maximum.value).withMods(^.id := "plannedTime")
              )
            else
              React.Fragment(
                FormLabel(htmlFor = "maxPlannedTime".refined)("Maximum Planned Time"),
                TimeSpanView(timeEstimateRange.maximum.value).withMods(^.id := "maxPlannedTime"),
                FormLabel(htmlFor = "minPlannedTime".refined)("Minimum Planned Time"),
                TimeSpanView(timeEstimateRange.minimum.value).withMods(^.id := "minPlannedTime")
              )
          ))

      val groupTypeSpecificForms =
        if isAnd then <.div(ExploreStyles.GroupForm)(nameForm, orderForm, delaysForm, plannedTime)
        else <.div(ExploreStyles.GroupForm)(nameForm, minRequiredForm, plannedTime)

      React.Fragment(
        props.renderInTitle(makeTitle(group, props.timeEstimateRange, props.elementsLength)),
        <.div(ExploreStyles.GroupEditTile)(
          selectGroupForm,
          groupTypeSpecificForms
        )
      )

  private def makeTitle(
    group:             GroupTree.Group,
    timeEstimateRange: Pot[Option[ProgramTimeRange]],
    elementsLength:    Int
  ) =
    val timeStr: VdomNode  = timeEstimateRange.renderReady(_.map: timeEstimateRange =>
      if timeEstimateRange.maximum === timeEstimateRange.minimum then
        HoursMinutesAbbreviation.format(timeEstimateRange.maximum.value)
      else
        s"${HoursMinutesAbbreviation.format(timeEstimateRange.maximum.value)} max - ${HoursMinutesAbbreviation
            .format(timeEstimateRange.minimum.value)} min"
    .map(s => s", $s"))
    val andOrStr: VdomNode =
      if group.isAnd then if group.ordered then "Ordered" else "Any order"
      else s"Choose ${group.minimumRequired.getOrElse(1.refined[NonNegative])} of ${elementsLength}"

    <.span("(", andOrStr, timeStr, ")")
