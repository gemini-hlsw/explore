// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.data.Chain
import cats.effect.IO
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
import explore.syntax.ui.*
import explore.undo.UndoSetter
import explore.utils.ToastCtx
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
import cats.derived.*
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.scalajs.js
import scala.util.Try
import cats.kernel.Eq

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
    .renderWithReuse: (props, ctx, editType, isLoading) =>
      import ctx.given

      val group          = props.group.get
      val isAnd          = group.isAnd
      val elementsLength = group.elements.length

      val isDisabled = isLoading.get || elementsLength <= 1

      val setMinimumRequired: Option[NonNegShort] => Callback = props.group.set(
        Grouping.minimumRequired.get,
        Grouping.minimumRequired.replace,
        mmin =>
          GroupQueries
            .updateGroup[IO](group.id, GroupPropertiesInput(minimumRequired = mmin.orUnassign))
            .switching(isLoading.async)
      )
      val setName: Option[NonEmptyString] => Callback         = props.group.set(
        Grouping.name.get,
        Grouping.name.replace,
        n =>
          GroupQueries
            .updateGroup[IO](group.id, GroupPropertiesInput(name = n.orUnassign))
            .switching(isLoading.async)
      )
      val setOrdered: Boolean => Callback                     = o =>
        GroupQueries.updateGroup[IO](group.id, GroupPropertiesInput(ordered = o.assign)).runAsync
      val setMinimumDelay: Option[TimeSpan] => Callback       = props.group.set(
        Grouping.minimumInterval.get,
        Grouping.minimumInterval.replace,
        m =>
          GroupQueries
            .updateGroup[IO](
              group.id,
              GroupPropertiesInput(minimumInterval =
                m.map(ts => TimeSpanInput(microseconds = ts.toMicroseconds.assign)).orUnassign
              )
            )
            .switching(isLoading.async)
      )
      val setMaximumDelay: Option[TimeSpan] => Callback       = props.group.set(
        Grouping.maximumInterval.get,
        Grouping.maximumInterval.replace,
        m =>
          GroupQueries
            .updateGroup[IO](
              group.id,
              GroupPropertiesInput(maximumInterval =
                m.map(ts => TimeSpanInput(microseconds = ts.toMicroseconds.assign)).orUnassign
              )
            )
            .switching(isLoading.async)
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
          onChange = tp => setMinimumRequired(minimumForGroup(tp))
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
          value = group.name.fold(js.undefined)(_.value),
          disabled = isDisabled,
          onChange = e => setName(NonEmptyString.from(e.target.value).toOption)
        )
      )

      val minRequiredForm = <.div(
        "Observe ",
        InputText(
          "minRequiredInput",
          placeholder = "1",
          clazz = ExploreStyles.MinRequiredFormInput,
          value = group.minimumRequired.fold(js.undefined)(_.value.toString),
          disabled = isDisabled,
          modifiers = Seq(^.`type` := "number", ^.min := 0, ^.max := elementsLength),
          onChange = e =>
            val newMin = e.target.value.toShortOption
              .map(s => Math.abs(Math.min(s, elementsLength)).toShort)
              .flatMap(s => NonNegShort.from(s).toOption)
            setMinimumRequired(newMin)
        ),
        s" of ${elementsLength} observations"
      )

      val orderForm = <.div(
        CheckboxView(
          id = "orderedCheck".refined,
          value = props.group.undoableView(Grouping.ordered).withOnMod(setOrdered(_)),
          label = "Ordered",
          disabled = isDisabled
        )
      )

      val delaysForm = <.div(
        FormInputText(
          id = "minDelay".refined,
          label = "Minimum Delay",
          value = group.minimumInterval.fold(js.undefined)(timeSpanToStringParts),
          disabled = isDisabled,
          onBlur = e =>
            parseTimeStringParts(e.target.value)
              .fold(e => ToastCtx[IO].showToast(e).runAsync, ts => setMinimumDelay(ts.some))
        ),
        FormInputText(
          id = "maxDelay".refined,
          label = "Maximum Delay",
          value = group.maximumInterval.fold(js.undefined)(timeSpanToStringParts),
          disabled = isDisabled,
          onBlur = e =>
            parseTimeStringParts(e.target.value)
              .fold(e => ToastCtx[IO].showToast(e).runAsync, ts => setMaximumDelay(ts.some))
        )
      )

      val groupTypeSpecificForms =
        if isAnd then <.div(nameForm, orderForm, delaysForm)
        else <.div(nameForm, minRequiredForm)

      // React.Fragment(
      //   props.renderInTitle(title),
      <.div(ExploreStyles.GroupEditTile)(
        <.div("Add at least 2 elements to this group to change the type.")
          .when(elementsLength <= 1),
        selectGroupForm,
        groupTypeSpecificForms
      )
  // )

  def parseTimeStringParts(str: String): Either[String, TimeSpan] =
    val parts = str.split(' ').map(_.trim()).filterNot(_.isBlank())
    if parts.isEmpty then TimeSpan.Zero.asRight
    else
      parts
        .foldLeft(TimeSpan.Zero.asRight)((acc, part) =>
          Try(Duration(part)).toEither.leftMap(_.getMessage) match
            case Left(e)       => s"Invalid time format ${part}: $e".asLeft
            case Right(parsed) =>
              acc.map(acc => acc +| TimeSpan.unsafeFromMicroseconds(parsed.toMicros))
        )

  def timeSpanToStringParts(ts: TimeSpan): String = {
    val units =
      Seq(TimeUnit.DAYS, TimeUnit.HOURS, TimeUnit.MINUTES, TimeUnit.SECONDS, TimeUnit.MILLISECONDS)

    val timeStrings = units
      .foldLeft((Chain.empty[String], ts.toMilliseconds)) { case ((humanReadable, rest), unit) =>
        val name   = unit.toString().toLowerCase() match {
          case "days"         => "d"
          case "hours"        => "h"
          case "minutes"      => "m"
          case "seconds"      => "s"
          case "milliseconds" => "ms"
        }
        val result = unit.convert(rest.toLong, TimeUnit.MILLISECONDS)
        val diff   = rest - TimeUnit.MILLISECONDS.convert(result, unit)
        val str    = result match {
          case 0    => humanReadable
          case more => humanReadable :+ s"$more$name"
        }
        (str, diff)
      }
      ._1

    timeStrings match {
      case Chain() => "0s"
      case _       => timeStrings.mkString_(" ")
    }
  }
