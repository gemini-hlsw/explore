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
import explore.components.FormTimeSpanInput
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Grouping
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
import monocle.Lens

import java.util.concurrent.TimeUnit
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

  private given Display[GroupEditType]                 = Display.byShortName(_.tag)
  private given Reusability[GroupEditType]             = Reusability.byEq
  private given Reusability[Grouping]                  = Reusability.byEq
  private given Reusability[Props]                     = Reusability.by(_.group.get)
  private given [A: Reusability]: Reusability[View[A]] = Reusability.by(_.get)

  private def minimumForGroup(t: GroupEditType): Option[NonNegShort] =
    if t == GroupEditType.And then none
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
    .useEffectOnMountBy((props, ctx, editType, isLoading) =>
      Callback.log("GroupEditTile is mounting. isLoading: ", isLoading.get)
    )
    .useState(0)
    .renderWithReuse: (props, ctx, editType, isLoading, inc) =>
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
      val minIntervalV = groupModView(
        Grouping.minimumInterval,
        t =>
          GroupPropertiesInput(minimumInterval =
            t.map(ts => TimeSpanInput(microseconds = ts.toMicroseconds.assign)).orUnassign
          )
      )
      val maxIntervalV = groupModView(
        Grouping.maximumInterval,
        t =>
          GroupPropertiesInput(maximumInterval =
            t.map(ts => TimeSpanInput(microseconds = ts.toMicroseconds.assign)).orUnassign
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
        <.span("Minimum delay"),
        FormTimeSpanInput(
          value = minIntervalV.get.getOrElse(TimeSpan.Zero),
          onChange = e => minIntervalV.set(e.some),
          disabled = isDisabled
        ),
        <.span("Maximum delay"),
        FormTimeSpanInput(
          value = maxIntervalV.get.getOrElse(TimeSpan.Zero),
          onChange = e => maxIntervalV.set(e.some),
          disabled = isDisabled
        )
      )

      val groupTypeSpecificForms =
        if isAnd then <.div(ExploreStyles.GroupForm, nameForm, orderForm, delaysForm)
        else <.div(ExploreStyles.GroupForm, nameForm, minRequiredForm)

      React.Fragment(
        props.renderInTitle(title),
        <.div(ExploreStyles.GroupEditTile)(
          <.div("Add at least 2 elements to this group to change the type.")
            .when(elementsLength <= 1),
          selectGroupForm,
          <.button(
            ^.disabled := isLoading.get,
            ^.onClick --> inc.modState(_ + 1),
            s"Increment ${inc.value}"
          ),
          groupTypeSpecificForms
        )
      )
