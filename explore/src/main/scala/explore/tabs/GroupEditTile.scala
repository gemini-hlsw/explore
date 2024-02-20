// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.derived.*
import cats.effect.IO
import cats.kernel.Eq
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.GroupQueries
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Grouping
import explore.model.syntax.all.toHoursMinutes
import explore.syntax.ui.*
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.react.*
import lucuma.react.common.ReactFnProps
import lucuma.react.common.*
import lucuma.react.primereact.InputText
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.GroupPropertiesInput
import lucuma.schemas.ObservationDB.Types.TimeSpanInput
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import monocle.Iso
import monocle.Lens

import scala.scalajs.js

case class GroupEditTile(
  group:         UndoSetter[Grouping],
  renderInTitle: Tile.RenderInTitle
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

  def component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    // editType
    .useStateViewBy((props, _) =>
      if props.group.get.isAnd then GroupEditType.And else GroupEditType.Or
    )
    // isLoading
    .useStateView(false)
    .render: (props, ctx, editType, isLoading) =>
      import ctx.given

      val group          = props.group.get
      val isAnd          = group.isAnd
      val elementsLength = group.elements.length

      val isDisabled = isLoading.get || elementsLength <= 1

      def groupModView[A](lens: Lens[Grouping, A], prop: A => GroupPropertiesInput) = props.group
        .undoableView(lens)
        .withOnMod(a =>
          GroupQueries.updateGroup[IO](group.id, prop(a)).switching(isLoading.async).runAsync
        )

      val minRequiredV = groupModView(
        Grouping.minimumRequired,
        m => GroupPropertiesInput(minimumRequired = m.orUnassign)
      )
      val nameV        = groupModView(
        Grouping.name,
        n => GroupPropertiesInput(name = n.orUnassign)
      )
      val orderedV     = groupModView(Grouping.ordered, o => GroupPropertiesInput(ordered = o.assign))

      val timeSpanOrEmptyLens =
        Iso[Option[TimeSpan], TimeSpan](_.orEmpty)(_.some.filterNot(_.isZero))
      val minIntervalV        = groupModView(
        Grouping.minimumInterval.andThen(timeSpanOrEmptyLens),
        ts =>
          GroupPropertiesInput(minimumInterval =
            TimeSpanInput(microseconds = ts.toMicroseconds.assign).assign
          )
      )
      val maxIntervalV        = groupModView(
        Grouping.maximumInterval.andThen(timeSpanOrEmptyLens),
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
          label = <.div(ExploreStyles.GroupChangeLabel, "Type"),
          disabled = isDisabled,
          itemTemplate = _.value match
            case GroupEditType.And => <.span("AND (Scheduling)")
            case GroupEditType.Or  => <.span("OR (Choose ", <.i("n"), ")")
          ,
          onChange = tp => minRequiredV.set(minimumForGroup(tp))
        )
      )

      val title: VdomNode =
        if isAnd then EmptyVdom
        else
          s"(Choose ${group.minimumRequired.getOrElse(NonNegShort.unsafeFrom(1))} of ${elementsLength})"

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
        ExploreStyles.GroupNameForm,
        FormInputText(
          id = "nameInput".refined,
          label = "Name",
          value = nameV.get.fold(js.undefined)(_.value),
          disabled = isDisabled,
          onChange = e => nameV.set(NonEmptyString.from(e.target.value).toOption)
        )
      )

      val minRequiredForm = <.div(
        "Observe ",
        InputText(
          "minRequiredInput",
          placeholder = "1",
          clazz = ExploreStyles.MinRequiredFormInput,
          value = minRequiredV.get.fold(js.undefined)(_.value.toString),
          disabled = isDisabled,
          modifiers = Seq(^.`type` := "number", ^.min := 0, ^.max := elementsLength),
          onChange = e =>
            val newMin = e.target.value.toShortOption
              .map(s => Math.abs(Math.min(s, elementsLength)).toShort)
              .flatMap(s => NonNegShort.from(s).toOption)
            minRequiredV.set(newMin)
        ),
        s" of ${elementsLength} observations"
      )

      val orderForm = <.div(
        CheckboxView(
          id = "orderedCheck".refined,
          value = orderedV,
          label = "Ordered",
          disabled = isDisabled
        )
      )

      val delaysForm = <.div(
        ExploreStyles.GroupDelaysForm,
        FormLabel(htmlFor = "minDelay".refined)("Minimum delay"),
        FormTimeSpanInput(value = minIntervalV, id = "maxDelay".refined, disabled = isDisabled),
        FormLabel(htmlFor = "maxDelay".refined)("Maximum delay"),
        FormTimeSpanInput(value = maxIntervalV, id = "maxDelay".refined, disabled = isDisabled)
      )

      val plannedTime =
        group.timeEstimateRange.map: timeEstimateRange =>
          <.div(ExploreStyles.GroupPlannedTime)(
            if timeEstimateRange.maximum === timeEstimateRange.minimum then
              React.Fragment(
                FormLabel(htmlFor = "plannedTime".refined)("Planned Time"),
                <.span(^.id := "plannedTime", timeEstimateRange.maximum.value.toHoursMinutes)
              )
            else
              React.Fragment(
                FormLabel(htmlFor = "maxPlannedTime".refined)("Maximum Planned Time"),
                <.span(^.id := "maxPlannedTime", timeEstimateRange.maximum.value.toHoursMinutes),
                FormLabel(htmlFor = "minPlannedTime".refined)("Minimum Planned Time"),
                <.span(^.id := "minPlannedTime", timeEstimateRange.minimum.value.toHoursMinutes)
              )
          )

      val groupTypeSpecificForms =
        if isAnd then <.div(ExploreStyles.GroupForm, nameForm, orderForm, delaysForm, plannedTime)
        else <.div(ExploreStyles.GroupForm, nameForm, minRequiredForm, plannedTime)

      React.Fragment(
        props.renderInTitle(title),
        <.div(ExploreStyles.GroupEditTile)(
          <.div("Add at least 2 elements to this group to change the type.")
            .when(elementsLength <= 1),
          selectGroupForm,
          groupTypeSpecificForms
        )
      )
