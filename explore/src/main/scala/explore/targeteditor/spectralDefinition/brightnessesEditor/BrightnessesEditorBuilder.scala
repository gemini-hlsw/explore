// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition.brightnessesEditor

import cats.Order.*
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.components.ui.ExploreStyles
import explore.model.ExploreModelValidators
import explore.model.display.given
import explore.utils.IsExpanded
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.dimensional.*
import lucuma.core.util.Enumerated
import lucuma.core.util.Of
import lucuma.react.primereact.Button
import lucuma.react.primereact.Panel
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.utils.*
import monocle.Focus

import scala.collection.immutable.SortedMap

private abstract class BrightnessesEditorBuilder[T, Props <: BrightnessesEditor[T]](implicit
  enumUnits: Enumerated[Units Of Brightness[T]]
):
  protected case class State(usedBands: Set[Band], newBand: Option[Band])

  object State {
    val usedBands = Focus[State](_.usedBands)
    val newBand   = Focus[State](_.newBand)

    def fromUsedBrightnesses(brightnesses: SortedMap[Band, BrightnessMeasure[T]]): State = {
      val usedBands = brightnesses.keySet
      State(usedBands, Band.all.diff(usedBands.toList).headOption)
    }
  }

  protected val label: String // Abstract

  protected def defaultBandUnits: Band => Units Of Brightness[T] // Abstract

  private type RowValue = (Band, View[BrightnessMeasure[T]])

  protected[targeteditor] case class TableMeta(
    disabled:        Boolean,
    modBrightnesses: (
      SortedMap[Band, BrightnessMeasure[T]] => SortedMap[Band, BrightnessMeasure[T]]
    ) => Callback
  )

  private val ColDef = ColumnDef[RowValue].WithTableMeta[TableMeta]

  private val BandColumnId: ColumnId   = ColumnId("band")
  private val ValueColumnId: ColumnId  = ColumnId("value")
  private val UnitsColumnId: ColumnId  = ColumnId("units")
  private val DeleteColumnId: ColumnId = ColumnId("delete")

  private val Columns =
    Reusable.always:
      List(
        ColDef(
          BandColumnId,
          _._1,
          "Band",
          _.value.shortName,
          size = 67.toPx
        ).sortable,
        ColDef(
          ValueColumnId,
          _._2.zoom(Measure.valueTagged[BrightnessValue, Brightness[T]]),
          "Value",
          cell =>
            FormInputTextView(
              id = NonEmptyString.unsafeFrom(s"brightnessValue_${cell.row.id}"),
              value = cell.value,
              validFormat = ExploreModelValidators.brightnessValidWedge,
              changeAuditor = ChangeAuditor.bigDecimal(2.refined, 3.refined).allowExp(2.refined),
              disabled = cell.table.options.meta.exists(_.disabled)
            ),
          size = 77.toPx
        ).sortableBy(_.get),
        ColDef(
          UnitsColumnId,
          _._2.zoom(Measure.unitsTagged[BrightnessValue, Brightness[T]]),
          "Units",
          cell =>
            EnumDropdownView(
              id = NonEmptyString.unsafeFrom(s"brightnessUnits_${cell.row.id}"),
              value = cell.value,
              disabled = cell.table.options.meta.exists(_.disabled),
              clazz = ExploreStyles.BrightnessesTableUnitsDropdown
            ),
          size = 145.toPx
        ).sortableBy(_.get),
        ColDef(
          DeleteColumnId,
          _._1,
          "",
          cell =>
            <.div(ExploreStyles.BrightnessesTableDeletButtonWrapper)(
              Button(
                icon = Icons.Trash,
                clazz = ExploreStyles.DeleteButton,
                text = true,
                disabled = cell.table.options.meta.exists(_.disabled),
                onClick = cell.table.options.meta.map(_.modBrightnesses(_ - cell.value)).orEmpty
              ).small
            ),
          size = 20.toPx,
          enableSorting = false
        )
      )

  protected[targeteditor] val component =
    ScalaFnComponent[Props]: props =>
      for
        state <- useStateView(State.fromUsedBrightnesses(props.brightnesses.get))
        _     <- useEffectWithDeps(props.brightnesses.get): brightnesses =>
                   state.set(State.fromUsedBrightnesses(brightnesses))
        rows  <- useMemo(props.brightnesses.get): _ =>
                   props.brightnesses.widen[Map[Band, BrightnessMeasure[T]]].toListOfViews
        table <- useReactTable:
                   TableOptions(
                     Columns,
                     rows,
                     getRowId = (row, _, _) => RowId(row._1.tag),
                     enableSorting = true,
                     enableColumnResizing = true,
                     columnResizeMode = ColumnResizeMode.OnChange,
                     initialState =
                       TableState(sorting = Sorting(ColumnId("band") -> SortDirection.Ascending)),
                     meta = TableMeta(
                       disabled = props.disabled,
                       modBrightnesses = props.brightnesses.mod
                     )
                   )
      yield
        val footer =
          <.tr(
            <.td(
              ^.colSpan := 4,
              <.div(
                ExploreStyles.BrightnessesTableFooter,
                state
                  .zoom(State.newBand)
                  .mapValue { (bandView: View[Band]) =>
                    val addBrightness =
                      props.brightnesses.mod(brightnesses =>
                        brightnesses +
                          (bandView.get ->
                            defaultBandUnits(bandView.get)
                              .withValueTagged(BrightnessValue.unsafeFrom(0)))
                      )

                    React.Fragment(
                      EnumDropdownView(
                        id = "NEW_BAND".refined,
                        value = bandView,
                        exclude = state.get.usedBands,
                        clazz = ExploreStyles.FlatFormField, // TODO: Look at this CSS
                        disabled = props.disabled
                      ),
                      Button(
                        icon = Icons.New,
                        onClick = addBrightness,
                        disabled = props.disabled,
                        severity = Button.Severity.Secondary
                      ).mini.compact
                    )
                  }
                  .whenDefined
              )
            )
          )

        Panel(
          header = label,
          toggleable = true,
          onExpand = props.expanded.set(IsExpanded(true)),
          onCollapse = props.expanded.set(IsExpanded(false))
        )(
          <.div(ExploreStyles.ExploreTable |+| ExploreStyles.BrightnessesContainer)(
            PrimeAutoHeightVirtualizedTable(
              table,
              estimateSize = _ => 38.toPx,
              striped = true,
              compact = Compact.Very,
              footerMod = footer,
              tableMod = ExploreStyles.ExploreBorderTable,
              emptyMessage = "No brightnesses defined"
            )
          )
        )
