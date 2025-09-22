// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.data.NonEmptySet
import cats.effect.IO
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
import explore.services.OdbApi
import explore.syntax.ui.*
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.CalculatedValue
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.react.*
import lucuma.react.common.*
import lucuma.react.primereact.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.GroupPropertiesInput
import lucuma.schemas.odb.input.*
import lucuma.ui.components.TimeSpanView
import lucuma.ui.format.TimeSpanFormatter.HoursMinutesAbbreviation
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import monocle.Lens
import org.typelevel.log4cats.Logger

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

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

      val intervalsLens: Lens[Group, (Option[TimeSpan], Option[TimeSpan])] =
        Group.minimumInterval.disjointZip(Group.maximumInterval)

      def isIntervalError(min: Option[TimeSpan], max: Option[TimeSpan]): Boolean =
        // If either is unset, the API is fine with it
        (min, max).tupled.exists(_ > _)

      val intervalErrorString: NonEmptyString =
        "Mininum delay must be <= Maximum delay".refined

      def minimumForGroup(t: GroupEditType): Option[NonNegShort] =
        if t === GroupEditType.And then none
        else NonNegShort.from(1).toOption

      def groupModViewBase[A](
        group:     UndoSetter[Group],
        lens:      Lens[Group, A],
        prop:      A => GroupPropertiesInput,
        odbApi:    OdbApi[IO],
        isLoading: View[Boolean]
      )(using Logger[IO]) =
        props.group
          .undoableView(lens)
          .withOnMod(a =>
            odbApi.updateGroup(group.get.id, prop(a)).switching(isLoading.async).runAsync
          )

      for
        ctx           <- useContext(AppContext.ctx)
        editType      <- useStateView:
                           if props.group.get.isAnd then GroupEditType.And else GroupEditType.Or
        isLoading     <- useStateView(false)
        minIntervalV  <- useStateView(props.group.get.minimumInterval)
        maxIntervalV  <- useStateView(props.group.get.maximumInterval)
        _             <- useEffectWithDeps(props.group.get.minimumInterval): ots =>
                           minIntervalV.set(ots)
        _             <- useEffectWithDeps(props.group.get.maximumInterval): ots =>
                           maxIntervalV.set(ots)
        _             <-
          useEffectWithDeps((minIntervalV.get, maxIntervalV.get)): (min, max) =>
            if (
              !isIntervalError(min, max) &&
              (props.group.get.minimumInterval =!= min || props.group.get.maximumInterval =!= max)
            )
              groupModViewBase(
                props.group,
                intervalsLens,
                (n, x) =>
                  GroupPropertiesInput(minimumInterval = n.map(_.toInput).orUnassign,
                                       maximumInterval = x.map(_.toInput).orUnassign
                  ),
                ctx.odbApi,
                isLoading
              )(using ctx.logger).set((min, max))
            else Callback.empty
        nameDisplay   <- useState(props.group.get.name)
        intervalError <-
          useMemo((minIntervalV.get, maxIntervalV.get)): (min, max) =>
            Option.when(isIntervalError(min, max))(intervalErrorString).orUndefined
      yield
        import ctx.given

        val group = props.group.get
        val isAnd = group.isAnd

        val isDisabled: Boolean = props.readonly || isLoading.get

        def groupModView[A](lens: Lens[Group, A], prop: A => GroupPropertiesInput) =
          groupModViewBase(props.group, lens, prop, ctx.odbApi, isLoading)

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
            value = minIntervalV.withNoneAsEmpty,
            id = "minDelay".refined,
            label = "Minimum delay",
            min = TimeSpan.Zero,
            disabled = isDisabled,
            error = intervalError.value
          ),
          FormTimeSpanInput(
            value = maxIntervalV.withNoneAsEmpty,
            id = "maxDelay".refined,
            label = "Maximum delay",
            min = TimeSpan.Zero,
            disabled = isDisabled,
            error = intervalError.value
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
