// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import crystal.react.implicits._
import explore.common.TargetObsQueries
import explore.common.TargetObsQueries._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ExpandedIds
import explore.model.Focused
import explore.model.formats._
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.MagnitudeBand
import lucuma.core.math.Epoch
import lucuma.core.math.MagnitudeValue
import lucuma.core.math.Parallax
import lucuma.core.model.Magnitude
import lucuma.core.model.Target
import lucuma.ui.optics.TruncatedDec
import lucuma.ui.optics.TruncatedRA
import lucuma.ui.optics.ValidFormatInput
import react.common._
import react.semanticui.collections.table._
import react.semanticui.elements.icon.Icon
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.modules.dropdown._
import reactST.reactTable._
import reactST.reactTable.mod.IdType

import scalajs.js.JSConverters._

final case class TargetSummaryTable(
  pointingsWithObs: PointingsWithObs,
  hiddenColumns:    View[Set[String]],
  focused:          View[Option[Focused]],
  expandedIds:      View[ExpandedIds],
  renderInTitle:    Tile.RenderInTitle
)(implicit val ctx: AppContextIO)
    extends ReactProps[TargetSummaryTable](TargetSummaryTable.component)

object TargetSummaryTable {
  type Props = TargetSummaryTable

  protected val TargetTable = TableMaker[TargetResult].withSort

  import TargetTable.syntax._

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
    "parallax"     -> "Parallax",
    "morphology"   -> "Morphology",
    "sed"          -> "SED"
  ) ++ MagnitudeBand.all.map(m => (m.shortName + "mag", m.shortName + "Mag")).toMap

  protected class Backend {

    def render(props: Props) = {
      implicit val ctx = props.ctx

      def targetObservations(id: Target.Id): List[ObsResult] =
        props.pointingsWithObs.observations.toList.filter(_.pointing match {
          case Some(PointingTargetResult(tid)) => tid === id
          case _                               => false
        })

      def column[V](id: String, accessor: TargetResult => V) =
        TargetTable
          .Column(id, accessor)
          .setHeader(columnNames(id))

      val columns =
        (List(
          column("type", _ => Icon("star").rawNode),
          column(
            "name",
            target =>
              <.a(^.onClick ==> (_ =>
                    props.focused.set(Focused.FocusedTarget(target.id).some).runAsyncCB
                  ),
                  target.name.value
              ).rawElement
          ),
          column(
            "ra",
            (TargetObsQueries.baseCoordinatesRa.get _)
              .andThen(TruncatedRA.rightAscension.get)
              .andThen(ValidFormatInput.truncatedRA.reverseGet)
          ),
          column(
            "dec",
            (TargetObsQueries.baseCoordinatesDec.get _)
              .andThen(TruncatedDec.declination.get)
              .andThen(ValidFormatInput.truncatedDec.reverseGet)
          ),
          column("priority", _ => "")
        ) ++
          MagnitudeBand.all.map(m =>
            column(
              m.shortName + "mag",
              _.magnitudes.collectFirst {
                case Magnitude(value, band, _, _) if band === m =>
                  MagnitudeValue.fromString.reverseGet(value)
              }.orEmpty
            )
          )
          ++ List(
            column(
              "epoch",
              (TargetObsQueries.epoch.get _).andThen(e =>
                s"${e.scheme.prefix}${Epoch.fromStringNoScheme.reverseGet(e)}"
              )
            ),
            column(
              "pmra",
              (TargetObsQueries.pmRALens.get _)
                .andThen(_.map(pmRAFormat.reverseGet).orEmpty)
            ),
            column(
              "pmdec",
              (TargetObsQueries.pmDecLens.get _)
                .andThen(_.map(pmDecFormat.reverseGet).orEmpty)
            ),
            column(
              "parallax",
              (TargetObsQueries.pxLens.get _)
                .andThen(_.map(Parallax.milliarcseconds.get).map(_.toString).orEmpty)
            ),
            column(
              "morphology",
              _ => ""
            ),
            column(
              "sed",
              _ => ""
            ),
            column(
              "count",
              target => targetObservations(target.id).length
            ),
            column(
              "observations",
              target =>
                <.span(
                  targetObservations(target.id)
                    .map(obs =>
                      <.a(
                        ^.onClick ==> (_ =>
                          (props.focused
                            .set(Focused.FocusedObs(obs.id).some) >> props.expandedIds
                            .mod(ExpandedIds.targetIds.modify(_ + target.id))).runAsyncCB
                        ),
                        obs.id.toString()
                      )
                    )
                    .mkReactFragment(", ")
                ).rawElement
            )
          )).toJSArray

      val rawData = props.pointingsWithObs.targets.toList.toJSArray

      tableComponent(
        TableComponentProps(TargetTable.Options(columns, rawData),
                            props.hiddenColumns,
                            props.renderInTitle
        )
      )
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

  protected final case class TableComponentProps(
    options:          TargetTable.OptionsType,
    hiddenColumns:    View[Set[String]],
    renderInTitle:    Tile.RenderInTitle
  )(implicit val ctx: AppContextIO)
      extends ReactProps[TargetSummaryTable](TargetSummaryTable.component)

  // Horrible hack while we don't fully have hooks.
  // Reusability is handled in class component, instead of the need to useMemo.
  // Table is only rerendered when needed, thus avoiding the loop in react-table when passing unstable columns or data.
  protected val tableComponent =
    ScalaFnComponent[TableComponentProps] { props =>
      implicit val ctx = props.ctx

      val tableInstance = TargetTable.use(
        props.options.setInitialStateFull(
          TargetTable
            .State()
            .setHiddenColumns(
              props.hiddenColumns.get.toList
                .map(col => col: IdType[TargetResult])
                .toJSArray
            )
        )
      )

      <.div(
        props.renderInTitle(
          <.span(ExploreStyles.TitleStrip)(
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
                            props.hiddenColumns
                              .mod(cols => if (value) cols - colId else cols + colId)
                              .runAsyncCB
                        )
                      )
                    )
                  }
              )
            )
          )
        ),
        TargetTableComponent(
          table =
            Table(celled = true, selectable = true, striped = true, compact = TableCompact.Very)(),
          header = true,
          headerCell = TableHeaderCell()(^.textTransform.none, ^.whiteSpace.nowrap),
          cell = TableCell()(^.whiteSpace.nowrap)
        )(tableInstance)
      )
    }
}
