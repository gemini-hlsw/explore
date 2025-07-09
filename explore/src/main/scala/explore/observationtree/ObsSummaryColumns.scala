// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Order
import cats.Order.given
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Focused
import explore.model.Group
import explore.model.Observation
import explore.model.display.given
import explore.model.enums.AppTab
import explore.model.syntax.all.*
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.syntax.display.*
import lucuma.core.util.CalculatedValue
import lucuma.core.util.TimeSpan
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.tooltip.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.TargetWithId
import lucuma.ui.format.TimeSpanFormatter.HoursMinutesAbbreviation
import lucuma.ui.table.*

import scala.collection.immutable.TreeSeqMap

trait ObsSummaryColumns:
  import ObsSummaryRow.*

  private val ColDef = ColumnDef[Expandable[ObsSummaryRow]]

  protected val ObservationIdColumnId = ColumnId("observation_id")
  protected val GroupColumnId         = ColumnId("group")
  protected val StateColumnId         = ColumnId("state")
  protected val ScienceBandColumnId   = ColumnId("science_band")
  protected val ExpanderColumnId      = ColumnId("expander")
  protected val TargetTypeColumnId    = ColumnId("target_type")
  protected val TargetColumnId        = ColumnId("target")
  protected val ConstraintsColumnId   = ColumnId("constraints")
  protected val ConfigurationColumnId = ColumnId("configuration")
  protected val DurationColumnId      = ColumnId("duration")
  protected val RAColumnId            = ColumnId("ra")
  protected val DecColumnId           = ColumnId("dec")
  protected val SEDColumnId           = ColumnId("sed")
  // private val ValidationCheckColumnId = ColumnId("validation_check")
  // private val CompletionColumnId    = ColumnId("completion")
  // private val FindingChartColumnId  = ColumnId("finding_chart")
  // private val PriorityColumnId      = ColumnId("priority")
  // private val TimingWindowsColumnId = ColumnId("timing_windows")
  // private val ChargedTimeColumnId = ColumnId("charged_time")

  private val ColumnNames: TreeSeqMap[ColumnId, String] =
    TreeSeqMap(
      ExpanderColumnId      -> " ",
      ObservationIdColumnId -> "Observation Id",
      TargetTypeColumnId    -> "Target Type",
      TargetColumnId        -> "Targets",
      GroupColumnId         -> "Group",
      StateColumnId         -> "State",
      ScienceBandColumnId   -> "Science Band",
      RAColumnId            -> "RA",
      DecColumnId           -> "Dec",
      SEDColumnId           -> "SED",
      ConstraintsColumnId   -> "Constraints",
      ConfigurationColumnId -> "Configuration",
      DurationColumnId      -> "Duration"
      // ValidationCheckColumnId -> " ",
      // CompletionColumnId    -> "Completion",
      // FindingChartColumnId -> "Finding Chart",
      // Default hidden columns
      // PriorityColumnId      -> "Priority",
      // TimingWindowsColumnId -> "Scheduling Windows",
      // ChargedTimeColumnId -> "ChargedTime"
    )

  protected val ColumnsExcludedFromVisibility: Set[ColumnId] =
    Set(ExpanderColumnId)

  private val ColumnHeaderOverrides: Set[ColumnId] =
    Set(TargetTypeColumnId)

  // Columns to be shown in the column visibility selector. We exclude
  // the science band because we set that visibility below.
  protected val SelectableColumnNames: List[(ColumnId, String)] =
    ColumnNames.filterNot((k, _) => ColumnsExcludedFromVisibility.contains(k)).toList

  protected val DefaultColVisibility: ColumnVisibility =
    ColumnVisibility(
      RAColumnId  -> Visibility.Hidden,
      DecColumnId -> Visibility.Hidden,
      SEDColumnId -> Visibility.Hidden
      // PriorityColumnId      -> Visibility.Hidden,
      // TimingWindowsColumnId -> Visibility.Hidden,
      // ChargedTimeColumnId -> Visibility.Hidden
    )

  def columns(pid: Program.Id, ctx: AppContext[IO]): List[ColDef.Type] =
    // For columns that only have data in the base observation row.
    def obsColumn[V](id: ColumnId, accessor: ObsRow => V): ColDef.TypeFor[Option[V]] =
      ColDef(
        id,
        v => v.value.fold(_ => none, accessor(_).some),
        if (ColumnHeaderOverrides.contains(id)) " " else ColumnNames(id)
      )

    extension [A](name: String | (A, TargetWithId))
      def sortableValue =
        name match
          case s: String => s
          case (_, b)    => b.target.name.value

    extension (a: Option[CalculatedValue[Option[TimeSpan]]])
      def sortableTimeSpan: Option[TimeSpan] = a.flatMap(_.value)

    // Function for sorting the observation by observation ref index (if available) or
    // observation id. If either observation has an index, both should. The Observations
    // are Optional because of the Rows, but all "top level" rows have an Observation, so
    // it doesn't matter what we return for the case of None.
    val identifierSortFn: (Option[Observation], Option[Observation]) => Int = (oo1, oo2) =>
      (oo1, oo2) match
        case (Some(o1), Some(o2)) =>
          (o1.reference, o2.reference)
            .mapN: (r1, r2) =>
              Order[PosInt].compare(r1.observationIndex, r2.observationIndex)
            .getOrElse(Order[Observation.Id].compare(o1.id, o2.id))
        case _                    => 0

    // Column with expanded accessor. For rows that have data in the expanded target row.
    def mixedColumn[V](
      id:               ColumnId,
      accessor:         ObsRow => V,
      expandedAccessor: ExpandedTargetRow => V
    ): ColDef.TypeFor[V] =
      ColDef(id, v => v.value.fold(expandedAccessor, accessor), ColumnNames(id))

    def constraintUrl(constraintId: Observation.Id): String =
      ctx.pageUrl((AppTab.Constraints, pid, Focused.singleObs(constraintId)).some)

    def goToConstraint(constraintId: Observation.Id): Callback =
      ctx.pushPage(
        (AppTab.Constraints, pid, Focused.singleObs(constraintId)).some
      )

    def targetLink(obsId: Observation.Id, tWId: TargetWithId): VdomNode =
      val text = tWId.target.name.value
      ctx.routingLink(
        (AppTab.Observations, pid, Focused.singleObs(obsId, tWId.id.some)).some,
        text
      )

    // Displays the link to the observation. If the observation has a reference, the
    // contents are the reference index, otherwise the observation id is shown.
    def obsLink(obs: Observation): VdomNode =
      ctx.obsIdRoutingLink(
        pid,
        obs.id,
        contents = obs.reference.map(o =>
          // The style sets the width and aligns to the right.
          <.div(ExploreStyles.ObservationsSummaryIndexCol, "%6d".format(o.observationIndex.value))
        )
      )

    def groupLink(group: Group): VdomNode =
      val text = group.name.map(_.toString).getOrElse(group.id.toString)
      ctx.routingLink(
        (AppTab.Observations, pid, Focused.group(group.id)).some,
        text
      )

    List(
      ColDef(
        ExpanderColumnId,
        cell = cell =>
          if (cell.row.getCanExpand())
            <.span(
              ^.cursor.pointer,
              TableStyles.ExpanderChevron,
              TableStyles.ExpanderChevronOpen.when(cell.row.getIsExpanded()),
              ^.onClick ==> (_.stopPropagationCB *> cell.row.getToggleExpandedHandler())
            )(TableIcons.ChevronRight.withFixedWidth(true))
          else "",
        enableResizing = false
      ).withSize(35.toPx),
      obsColumn(ObservationIdColumnId, _.obs)
        .withCell:
          _.value.map(obsLink)
        .sortableWith(identifierSortFn),
      // TODO: TargetTypeColumnId
      obsColumn(TargetTypeColumnId, _ => ())
        .withCell(_ => Icons.Star.withFixedWidth())
        .withSize(35.toPx)
        .sortable,
      mixedColumn(
        TargetColumnId,
        r => r.obs.title,
        r => (r.obs.id, r.targetWithId)
      )
        .withCell:
          _.value match
            case s: String => <.span(s)
            case (a, b)    => targetLink(a, b)
        .sortableBy(_.sortableValue),
      obsColumn(GroupColumnId, _.group)
        .withCell:
          _.value.flatten.map(groupLink)
        .sortableBy(_.flatMap(_.flatMap(_.name))),
      // TODO: ValidationCheckColumnId
      obsColumn(StateColumnId, _.obs.workflow.value.state)
        .withCell(_.value.map(_.toString).orEmpty)
        .sortable,
      obsColumn(ScienceBandColumnId, _.obs.scienceBand)
        .withCell:
          _.value.flatten.fold("Not set")(_.shortName)
        .sortable,
      // TODO: CompletionColumnId
      mixedColumn(
        RAColumnId,
        // at visualization time, defaults to base coordinates
        r => r.coordsAtVizTime.map(_.ra),
        r => r.coordsAtVizTime.map(_.ra)
      )
        .withCell(_.value.map(MathValidators.truncatedRA.reverseGet).orEmpty)
        .sortable,
      mixedColumn(
        DecColumnId,
        // at visualization time, defaults to base coordinates
        r => r.coordsAtVizTime.map(_.dec),
        r => r.coordsAtVizTime.map(_.dec)
      )
        .withCell(_.value.map(MathValidators.truncatedDec.reverseGet).orEmpty)
        .sortable,
      // TODO: TimingColumnId
      // TODO: SEDColumnId
      ColDef(
        SEDColumnId,
        v =>
          v.value
            .fold(_.targetWithId.target.some, _.targetWithId.map(_.target))
            .flatMap(Target.sidereal.getOption)
            .flatMap: t =>
              Target.Sidereal.integratedSpectralDefinition
                .getOption(t)
                .map(_.shortName)
                .orElse(Target.Sidereal.surfaceSpectralDefinition.getOption(t).map(_.shortName)),
        ColumnNames(SEDColumnId)
      ).withCell(cell =>
        cell.value
          .filterNot(_ => cell.row.getCanExpand())
          .orEmpty
      ).sortable,
      obsColumn(ConstraintsColumnId, r => (r.obs.id, r.obs.constraints.summaryString))
        .withCell: cell =>
          cell.value.map: (id, constraintsSummary) =>
            <.a(
              ^.href := constraintUrl(id),
              ^.onClick ==> (_.preventDefaultCB *> goToConstraint(id)),
              constraintsSummary
            ).withTooltip(content = constraintsSummary, position = Tooltip.Position.Top)
        .sortableBy(_.map(_._2)),
      // TODO: FindingChartColumnId
      obsColumn(ConfigurationColumnId, _.obs.basicConfiguration.foldMap(_.shortName))
        .withCell(cell =>
          val tt: Option[VdomNode] = cell.value.map(identity)
          <.span(cell.value.orEmpty)
            .withOptionalTooltip(content = tt, position = Tooltip.Position.Top)
        )
        .sortable,
      obsColumn(
        DurationColumnId,
        _.obs.execution.digest.programTimeEstimate
      ).withCell:
        _.value.map: cv =>
          cv.value.map: d =>
            val text = HoursMinutesAbbreviation.format(d)
            <.span(text, cv.staleClass)
              .withOptionalTooltip(content = cv.staleTooltip, position = Tooltip.Position.Left)
      .sortableBy(_.sortableTimeSpan)
      // TODO: PriorityColumnId
      // TODO: ChargedTimeColumnId
    )
  end columns
