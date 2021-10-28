// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order._
import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.TargetListGroupQueries._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.FocusedObs
import explore.model.SelectedPanel
import explore.model.TargetEnv
import explore.model.TargetEnvIdObsIdSet
import explore.model.TargetIdSet
import explore.model.conversions._
import explore.model.formats._
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.MagnitudeBand
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.math.MagnitudeValue
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.model.Magnitude
import lucuma.core.model.NonsiderealTarget
import lucuma.core.model.Observation
import lucuma.core.model.SiderealTarget
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.ui.optics.TruncatedDec
import lucuma.ui.optics.TruncatedRA
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._
import react.semanticui.collections.table._
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.modules.dropdown._
import reactST.reactTable._
import reactST.reactTable.mod.DefaultSortTypes
import reactST.reactTable.mod.IdType

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import scalajs.js.JSConverters._

final case class TargetSummaryTable(
  targetListGroupWithObs: TargetListGroupWithObs,
  hiddenColumns:          View[Set[String]],
  selectedPanel:          View[SelectedPanel[TargetEnvIdObsIdSet]],
  focusedObs:             View[Option[FocusedObs]],
  expandedIds:            View[SortedSet[TargetEnvIdObsIdSet]],
  renderInTitle:          Tile.RenderInTitle
) extends ReactFnProps[TargetSummaryTable](TargetSummaryTable.component)

final case class TargetEnvInfo(targetEnvId: TargetEnvIdObsIdSet) {
  lazy val observationIds: List[Observation.Id] = targetEnvId.value.toList.flatMap(_._2)
}

final case class TargetRow(
  id:            String,
  name:          NonEmptyString,
  tracking:      Option[SiderealTracking],
  magnitudes:    SortedMap[MagnitudeBand, Magnitude],
  targetEnvInfo: Option[TargetEnvInfo]
)

object TargetRow {
  def expandableFromTarget(
    id:            TargetIdSet,
    target:        Target,
    targetEnvInfo: Option[TargetEnvInfo]
  ): Expandable[TargetRow] =
    Expandable(
      // TODO Better toString for TargetIdSet
      target match {
        case SiderealTarget(name, tracking, magnitudes) =>
          TargetRow(id.toString, name, tracking.some, magnitudes, targetEnvInfo)
        case NonsiderealTarget(name, _, magnitudes)     =>
          TargetRow(id.toString, name, none, magnitudes, targetEnvInfo)
      }
    )

  def expandableFromTargetEnv(targetEnv: TargetEnv): Option[Expandable[TargetRow]] = {
    val targetEnvInfo = TargetEnvInfo(targetEnv.id)
    targetEnv.scienceTargets.toList match {
      case Nil                              => none
      case (targetIds, singleTarget) :: Nil =>
        expandableFromTarget(targetIds, singleTarget, targetEnvInfo.some).some
      case targets                          =>
        // TODO Better toString for TargetEnvIdObsIdSet
        Expandable(
          TargetRow(targetEnv.id.toString,
                    targetEnv.name,
                    none,
                    SortedMap.empty,
                    targetEnvInfo.some
          )
        ).withSubRows(targets.map { case (id, target) => expandableFromTarget(id, target, none) })
          .some
    }
  }
}

object TargetSummaryTable {
  type Props = TargetSummaryTable

  protected val TargetTable = TableDef[Expandable[TargetRow]].withExpanded.withSortBy

  protected val TargetTableComponent = new SUITable(TargetTable)

  implicit protected val propsReuse: Reusability[Props] = Reusability.derive

  private val columnNames: Map[String, String] = Map(
    "type"         -> " ",
    "name"         -> "Name",
    "ra"           -> "RA",
    "dec"          -> "Dec",
    "priority"     -> "Priority",
    "count"        -> "Count",
    "observations" -> "Observations",
    "epoch"        -> "Epoch",
    "pmra"         -> "µ RA",
    "pmdec"        -> "µ Dec",
    "rv"           -> "RV",
    "z"            -> "z",
    "cz"           -> "cz",
    "parallax"     -> "Parallax",
    "morphology"   -> "Morphology",
    "sed"          -> "SED"
  ) ++ MagnitudeBand.all.map(m => (m.shortName + "mag", m.shortName + "Mag")).toMap

  private val columnClasses: Map[String, Css] = Map(
    "expander" -> ExploreStyles.Sticky,
    "type"     -> (ExploreStyles.Sticky |+| ExploreStyles.TargetSummaryType),
    "name"     -> (ExploreStyles.Sticky |+| ExploreStyles.TargetSummaryName)
  )

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // cols
      .useMemoBy(_ => ()) { props => _ =>
        def column[V](id: String, accessor: TargetRow => V) =
          TargetTable
            .Column(id, row => accessor(row.value))
            .setHeader(columnNames(id))

        List(
          TargetTable
            .Column("expander")
            .setCell(cell =>
              if (cell.row.canExpand)
                <.span(
                  cell.row.getToggleRowExpandedProps(),
                  if (cell.row.isExpanded.contains(true)) Icons.ChevronDown
                  else
                    <.span(
                      // The next line is temporary. We're just compensanting for the fact that column
                      // widths are being completely ignored for some reason we don't quite comprehend yet.
                      ^.paddingRight := "3px",
                      Icons.ChevronRight
                    )
                )
              else ""
            )
            .setDisableSortBy(true)
            .setWidth(35),
          TargetTable
            .Column("type")
            .setCell(cell =>
              <.span(
                ExploreStyles.TargetSummarySubRowCell.when_(cell.row.depth > 0),
                // The next line is temporary. We're just compensanting for the fact that column
                // widths are being completely ignored for some reason we don't quite comprehend yet.
                (^.paddingRight := "15px").when(cell.row.depth == 0),
                if (cell.row.subRows.isEmpty) Icons.Star
                else Icons.Stars
              )
            )
            .setWidth(50),
          column("name", _.name)
            .setCell(cell =>
              <.a(
                ExploreStyles.TargetSummarySubRowCell.when_(cell.row.depth > 0),
                ^.onClick ==> (_ =>
                  cell.row.original.value.targetEnvInfo
                    .map(targetEnvInfo => // TODO Allow jumping to a specific target?
                      props.expandedIds.mod(_ + targetEnvInfo.targetEnvId) >>
                        props.selectedPanel.set(SelectedPanel.editor(targetEnvInfo.targetEnvId))
                    )
                    .orEmpty
                ),
                cell.value.toString
              )
            )
            .setSortByFn(_.toString),
          column(
            "ra",
            _.tracking.map(SiderealTracking.baseCoordinates.andThen(Coordinates.rightAscension).get)
          ).setCell(
            _.value
              .map(
                TruncatedRA.rightAscension.get
                  .andThen(ValidFormatInput.truncatedRA.reverseGet)
              )
              .orEmpty
          ).setSortByAuto,
          column(
            "dec",
            _.tracking.map(SiderealTracking.baseCoordinates.andThen(Coordinates.declination).get)
          ).setCell(
            _.value
              .map(
                TruncatedDec.declination.get
                  .andThen(ValidFormatInput.truncatedDec.reverseGet)
              )
              .orEmpty
          ).setSortByAuto,
          column("priority", _ => "")
        ) ++
          MagnitudeBand.all.map(band =>
            column(
              band.shortName + "mag",
              _.magnitudes.get(band).map(_.value)
            ).setCell(_.value.map(MagnitudeValue.fromString.reverseGet).orEmpty).setSortByAuto
          ) ++
          List(
            column("epoch", _.tracking.map(SiderealTracking.epoch.get))
              .setCell(
                _.value
                  .map(epoch =>
                    s"${epoch.scheme.prefix}${Epoch.fromStringNoScheme.reverseGet(epoch)}"
                  )
                  .orEmpty
              )
              .setSortByAuto,
            column(
              "pmra",
              _.tracking.flatMap(SiderealTracking.properMotion.get).map(ProperMotion.ra.get)
            )
              .setCell(
                _.value.map(pmRAFormat.reverseGet).orEmpty
              )
              .setSortByAuto,
            column(
              "pmdec",
              _.tracking.flatMap(SiderealTracking.properMotion.get).map(ProperMotion.dec.get)
            )
              .setCell(_.value.map(pmDecFormat.reverseGet).orEmpty)
              .setSortByAuto,
            column("rv", _.tracking.flatMap(SiderealTracking.radialVelocity.get))
              .setCell(_.value.map(formatRV.reverseGet).orEmpty)
              .setSortByAuto,
            column("z",
                   _.tracking.flatMap(
                     (SiderealTracking.radialVelocity.get _).andThen(rvToRedshiftGet)
                   )
            )
              .setCell(_.value.map(formatZ.reverseGet).orEmpty)
              .setSortByAuto,
            column("cz",
                   _.tracking.flatMap(
                     (SiderealTracking.radialVelocity.get _).andThen(rvToARVGet)
                   )
            )
              .setCell(_.value.map(formatCZ.reverseGet).orEmpty)
              .setSortByAuto,
            column("parallax", _.tracking.flatMap(SiderealTracking.parallax.get))
              .setCell(_.value.map(Parallax.milliarcseconds.get).map(_.toString).orEmpty)
              .setSortByAuto,
            column("morphology", _ => ""),
            column("sed", _ => ""),
            column("count", _.targetEnvInfo.map(_.observationIds.length)) // TODO Right align
              .setCell(_.value.map(_.toString).orEmpty)
              .setSortType(DefaultSortTypes.number),
            column("observations", _.targetEnvInfo)
              .setCell(cell =>
                cell.value
                  .map(targetEnvInfo =>
                    <.span(
                      targetEnvInfo.observationIds
                        .map(obsId =>
                          <.a(
                            ^.onClick ==> (_ =>
                              props.focusedObs.set(FocusedObs(obsId).some) >>
                                props.expandedIds.mod(_ + targetEnvInfo.targetEnvId) >>
                                props.selectedPanel
                                  .set(SelectedPanel.editor(targetEnvInfo.targetEnvId))
                            ),
                            obsId.toString
                          )
                        )
                        .mkReactFragment(", ")
                    )
                  )
                  .getOrElse("")
              )
              .setDisableSortBy(true)
          )
      }
      // rows
      .useMemoBy((props, _) => props.targetListGroupWithObs)((_, _) =>
        _.targetListGroups.values.toList.flatMap(TargetRow.expandableFromTargetEnv)
      )
      .useTableBy((props, cols, rows) =>
        TargetTable(
          cols,
          rows,
          { (hiddenColumns: Set[String], options: TargetTable.OptionsType) =>
            options
              .setAutoResetSortBy(false)
              .setInitialState(
                TargetTable
                  .State()
                  .setHiddenColumns(
                    hiddenColumns.toList
                      .map(col => col: IdType[TargetEnvIdObsIdSet])
                      .toJSArray
                  )
              )
          }.reuseCurrying(props.hiddenColumns.get)
        )
      )
      .render((props, _, _, tableInstance) =>
        <.div(
          props.renderInTitle(
            <.span(ExploreStyles.TitleSelectColumns)(
              Dropdown(item = true,
                       simple = true,
                       pointing = Pointing.TopRight,
                       scrolling = true,
                       text = "Columns",
                       clazz = ExploreStyles.SelectColumns
              )(
                DropdownMenu()(
                  tableInstance.allColumns
                    .drop(2)
                    .toTagMod { column =>
                      val colId = column.id.toString
                      DropdownItem()(^.key := colId)(
                        <.div(
                          Checkbox(
                            label = columnNames(colId),
                            checked = column.isVisible,
                            onChange = (value: Boolean) =>
                              Callback(column.toggleHidden()) >>
                                props.hiddenColumns
                                  .mod(cols => if (value) cols - colId else cols + colId)
                          )
                        )
                      )
                    }
                )
              )
            )
          ),
          TargetTableComponent(
            table = Table(celled = true,
                          selectable = true,
                          striped = true,
                          compact = TableCompact.Very
            )(),
            header = true,
            headerCell = (col: TargetTable.ColumnType) =>
              TableHeaderCell(clazz = columnClasses.get(col.id.toString).orUndefined)(
                ^.textTransform.none,
                ^.whiteSpace.nowrap
              ),
            cell = (cell: TargetTable.CellType[_]) =>
              TableCell(clazz = columnClasses.get(cell.column.id.toString).orUndefined)(
                ^.whiteSpace.nowrap
              )
          )(tableInstance)
        )
      )
}
