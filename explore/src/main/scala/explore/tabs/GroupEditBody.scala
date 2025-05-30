// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.data.NonEmptySet
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Group
import explore.model.ProgramTimeRange
import explore.model.enums.GroupWarning
import explore.syntax.ui.*
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.CalculatedValue
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.react.*
import lucuma.react.common.*
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

case class GroupEditBody(
  group:          UndoSetter[Group],
  warnings:       Option[NonEmptySet[GroupWarning]],
  elementsLength: Int,
  readonly:       Boolean
) extends ReactFnProps(GroupEditBody):
  val timeEstimateRange: Option[CalculatedValue[Option[ProgramTimeRange]]] =
    group.get.timeEstimateRange

object GroupEditBody
    extends ReactFnComponent[GroupEditBody](props =>
      enum GroupEditType(val tag: String) derives Enumerated:
        case And extends GroupEditType("And")
        case Or  extends GroupEditType("Or")

      given Display[GroupEditType] = Display.byShortName(_.tag)

      def minimumForGroup(t: GroupEditType): Option[NonNegShort] =
        if t === GroupEditType.And then none
        else NonNegShort.from(1).toOption

      for
        ctx         <- useContext(AppContext.ctx)
        editType    <- useStateView:
                         if props.group.get.isAnd then GroupEditType.And else GroupEditType.Or
        isLoading   <- useStateView(false)
        nameDisplay <- useState(props.group.get.name)
      yield
        import ctx.given

        val group = props.group.get
        val isAnd = group.isAnd

        val isDisabled: Boolean = props.readonly || isLoading.get

        def groupModView[A](lens: Lens[Group, A], prop: A => GroupPropertiesInput) =
          props.group
            .undoableView(lens)
            .withOnMod(a =>
              ctx.odbApi.updateGroup(group.id, prop(a)).switching(isLoading.async).runAsync
            )

        val minRequiredV = groupModView(
          Group.minimumRequired,
          m => GroupPropertiesInput(minimumRequired = m.orUnassign)
        )
        val nameV        = groupModView(
          Group.name,
          n => GroupPropertiesInput(name = n.orUnassign)
        )
        val orderedV     =
          groupModView(Group.ordered, o => GroupPropertiesInput(ordered = o.assign))

        val timeSpanOrEmptyLens =
          Iso[Option[TimeSpan], TimeSpan](_.orEmpty)(_.some.filterNot(_.isZero))
        val minIntervalV        = groupModView(
          Group.minimumInterval.andThen(timeSpanOrEmptyLens),
          ts =>
            GroupPropertiesInput(minimumInterval =
              TimeSpanInput(microseconds = ts.toMicroseconds.assign).assign
            )
        )
        val maxIntervalV        = groupModView(
          Group.maximumInterval.andThen(timeSpanOrEmptyLens),
          ts =>
            GroupPropertiesInput(maximumInterval =
              TimeSpanInput(microseconds = ts.toMicroseconds.assign).assign
            )
        )

        val changeGroupTypeButtons =
          <.div(ExploreStyles.GroupChangeButtons)(
            SelectButtonEnumView(
              "groupType".refined,
              editType,
              label = "Type",
              disabled = isDisabled,
              itemTemplate = _.value match
                case GroupEditType.And => <.span("AND (Scheduling)")
                case GroupEditType.Or  => <.span("OR (Choose ", <.i("n"), ")")
              ,
              onChange = tp => minRequiredV.set(minimumForGroup(tp))
            )
          )

        val note =
          <.div(ExploreStyles.GroupEditNote)(
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

        val selectGroupForm =
          <.div(ExploreStyles.GroupTypeSelect)(
            changeGroupTypeButtons,
            note
          )

        val nameForm = <.div(
          FormInputText(
            id = "nameInput".refined,
            label = "Name",
            value = nameDisplay.value.fold(js.undefined)(_.value),
            disabled = isDisabled,
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
            disabled = isDisabled,
            min = 0,
            max = props.elementsLength,
            size = minRequiredV.get.fold(js.undefined)(_.toString.length),
            onValueChange = e =>
              val newMin = e.valueOption
                .flatMap(s => NonNegShort.from(s.toShort).toOption)
              minRequiredV.set(newMin)
          ),
          s" of ${props.elementsLength} observations/subgroups"
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
          FormTimeSpanInput(
            value = minIntervalV,
            id = "minDelay".refined,
            label = "Minimum delay",
            min = TimeSpan.Zero,
            max = maxIntervalV.get,
            disabled = isDisabled
          ),
          FormTimeSpanInput(
            value = maxIntervalV,
            id = "maxDelay".refined,
            label = "Maximum delay",
            min = minIntervalV.get,
            disabled = isDisabled
          )
        )

        val plannedTime =
          props.timeEstimateRange.flatMap: cvter =>
            val staleTooltip = cvter.staleTooltip
            val staleClass   = cvter.staleClass
            cvter.value.map: timeEstimateRange =>
              <.div(ExploreStyles.GroupPlannedTime)(
                if timeEstimateRange.maximum === timeEstimateRange.minimum then
                  React.Fragment(
                    FormLabel(htmlFor = "plannedTime".refined)("Planned Time"),
                    TimeSpanView(timeEstimateRange.maximum.value, tooltip = staleTooltip)
                      .withMods(^.id := "plannedTime", staleClass)
                  )
                else
                  React.Fragment(
                    FormLabel(htmlFor = "maxPlannedTime".refined)("Maximum Planned Time"),
                    TimeSpanView(timeEstimateRange.maximum.value, tooltip = staleTooltip)
                      .withMods(^.id := "maxPlannedTime", staleClass),
                    FormLabel(htmlFor = "minPlannedTime".refined)("Minimum Planned Time"),
                    TimeSpanView(timeEstimateRange.minimum.value, tooltip = staleTooltip)
                      .withMods(^.id := "minPlannedTime", staleClass)
                  )
              )

        val groupTypeSpecificForms =
          if isAnd then <.div(ExploreStyles.GroupForm)(nameForm, orderForm, delaysForm, plannedTime)
          else <.div(ExploreStyles.GroupForm)(nameForm, minRequiredForm, plannedTime)

        React.Fragment(
          <.div(ExploreStyles.GroupEditTile)(
            selectGroupForm,
            groupTypeSpecificForms,
            props.warnings.map: nes =>
              <.div(
                ExploreStyles.GroupWarnings,
                nes.toList
                  .toTagMod(using w => Message(id = s"${group.id}-${w.shortMsg}", text = w.longMsg, severity = Message.Severity.Warning))
              )
          )
        )
    )

case class GroupEditTitle(
  group:          UndoSetter[Group],
  elementsLength: Int
) extends ReactFnProps(GroupEditTitle):
  val timeEstimateRange: Option[CalculatedValue[Option[ProgramTimeRange]]] =
    group.get.timeEstimateRange

object GroupEditTitle
    extends ReactFnComponent[GroupEditTitle](props =>

      def makeTitle(
        group:             Group,
        timeEstimateRange: Option[CalculatedValue[Option[ProgramTimeRange]]],
        elementsLength:    Int
      ) =
        val timeStr: VdomNode  = timeEstimateRange.flatMap: cvter =>
          cvter.value.map: timeEstimateRange =>
            val str =
              if timeEstimateRange.maximum === timeEstimateRange.minimum then
                HoursMinutesAbbreviation.format(timeEstimateRange.maximum.value)
              else
                s"${HoursMinutesAbbreviation.format(timeEstimateRange.maximum.value)} max - ${HoursMinutesAbbreviation
                    .format(timeEstimateRange.minimum.value)} min"
            <.span(s", $str", ExploreStyles.GroupEditTitleTimes, cvter.staleClass)
        val andOrStr: VdomNode =
          if group.isAnd then if group.ordered then "Ordered" else "Any order"
          else
            s"Choose ${group.minimumRequired.getOrElse(1.refined[NonNegative])} of ${elementsLength}"

        <.span("(", andOrStr, timeStr, ")")

      for ctx <- useContext(AppContext.ctx)
      yield makeTitle(props.group.get, props.timeEstimateRange, props.elementsLength)
    )
