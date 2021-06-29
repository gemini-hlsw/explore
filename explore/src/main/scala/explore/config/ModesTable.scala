// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all._
import coulomb.Quantity
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.NonNegBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import explore.modes._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.F2Disperser
import lucuma.core.enum.GmosSouthDisperser
import lucuma.core.enum.GnirsDisperser
import lucuma.core.enum.GpiDisperser
import lucuma.core.math.units.Micrometer
import lucuma.core.util.Display
import react.common._
import react.common.implicits._
import react.semanticui.collections.table._
import reactST.reactTable._

import scalajs.js.JSConverters._

final case class SpectroscopyModesTable(
  matrix: SpectroscopyModesMatrix
) extends ReactProps[SpectroscopyModesTable](SpectroscopyModesTable.component)

object SpectroscopyModesTable {
  type Props = SpectroscopyModesTable
  type ColId = NonEmptyString

  protected val ModesTableMaker = TableMaker[SpectroscopyModeRow]

  import ModesTableMaker.syntax._

  protected val ModesTableComponent = new SUITable(ModesTableMaker)

  val disperserDisplay: Display[ModeDisperser] = Display.byShortName {
    case ModeDisperser.NoDisperser      => "-"
    case ModeDisperser.SomeDisperser(t) => t
  }

  def column[V](id: ColId, accessor: SpectroscopyModeRow => V) =
    ModesTableMaker
      .Column(id, accessor)
      .setHeader(columnNames.getOrElse(id, id.value): String)

  val InstrumentColumnId: ColId = "instrument"
  val ConfigColumnId: ColId     = "config"
  val SlitWidthColumnId: ColId  = "slit_width"
  val SlitLengthColumnId: ColId = "slit_length"
  val DisperserColumnId: ColId  = "disperser"
  val RangeColumnId: ColId      = "range"
  val ResolutionColumnId: ColId = "resolution"

  private val columnNames: Map[ColId, String] =
    Map[NonEmptyString, String](
      InstrumentColumnId -> "Instrument",
      ConfigColumnId     -> "Config",
      SlitWidthColumnId  -> "Slit Width",
      SlitLengthColumnId -> "Slit Length",
      DisperserColumnId  -> "Disperser",
      RangeColumnId      -> "Range",
      ResolutionColumnId -> "Res."
    )

  val formatSlitWidth: ModeSlitSize => String = ss =>
    f"${ModeSlitSize.milliarcseconds.get(ss.size).setScale(3, BigDecimal.RoundingMode.UP)}%1.3f"

  val formatSlitLength: ModeSlitSize => String = ss =>
    f"${ModeSlitSize.milliarcseconds.get(ss.size).setScale(0, BigDecimal.RoundingMode.DOWN)}%1.0f"

  def formatDisperser(disperser: InstrumentRow#Disperser): String = disperser match {
    case f: GmosSouthDisperser => f.shortName
    case f: F2Disperser        => f.shortName
    case f: GpiDisperser       => f.shortName
    case f: GnirsDisperser     => f.shortName
    case r                     => r.toString
  }

  def formatWavelengthRange(r: Quantity[NonNegBigDecimal, Micrometer]): String =
    f"${r.value.value.setScale(3, BigDecimal.RoundingMode.DOWN)}%1.0f"

  val columns =
    List(
      column(InstrumentColumnId, SpectroscopyModeRow.instrument.get)
        .setCell(_.value.shortName)
        .setWidth(30),
      column(ConfigColumnId, SpectroscopyModeRow.config.get)
        .setCell(_.value.value)
        .setWidth(30),
      column(SlitWidthColumnId, SpectroscopyModeRow.slitWidth.get)
        .setCell(c => formatSlitWidth(c.value))
        .setWidth(10),
      column(SlitLengthColumnId, SpectroscopyModeRow.slitLength.get)
        .setCell(c => formatSlitLength(c.value))
        .setWidth(10),
      column(DisperserColumnId, SpectroscopyModeRow.disperser.get)
        .setCell(c => formatDisperser(c.value))
        .setWidth(10),
      column(RangeColumnId, SpectroscopyModeRow.range.get)
        .setCell(c => formatWavelengthRange(c.value))
        .setWidth(10),
      column(ResolutionColumnId, SpectroscopyModeRow.resolution.get)
        .setCell(c => c.value.toString)
        .setWidth(10)
    )

  protected val component =
    ScalaComponent
      .builder[Props]
      .render_P { p =>
        tableComponent(
          ModesTableProps(
            ModesTableMaker.Options(columns.toJSArray, p.matrix.matrix.toJSArray)
          )
        )
      }
      // .configure(Reusability.shouldComponentUpdate)
      .build

  protected final case class ModesTableProps(
    options: ModesTableMaker.OptionsType
  ) extends ReactProps[SpectroscopyModesTable](SpectroscopyModesTable.component)

  protected val tableComponent =
    ScalaFnComponent[ModesTableProps] { props =>
      val tableInstance = ModesTableMaker.use(
        props.options
      )
      <.div(
        ExploreStyles.ModesTable,
        ModesTableComponent(
          table =
            Table(celled = true, selectable = true, striped = true, compact = TableCompact.Very)(),
          header = true,
          headerCell = (_: ModesTableMaker.ColumnType) =>
            TableHeaderCell(clazz = ExploreStyles.Sticky |+| ExploreStyles.ModesHeader)(
              ^.textTransform.capitalize,
              ^.whiteSpace.nowrap
            )
        )(tableInstance)
      )
    }

}
