// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all._
import explore.components.ui.ExploreStyles
import explore.modes._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.util.Display
import react.common._
import react.common.implicits._
import react.semanticui.collections.table._
import reactST.reactTable._

import scalajs.js.JSConverters._

final case class ModesTable(
  matrix: ModesMatrix
) extends ReactProps[ModesTable](ModesTable.component)

object ModesTable {
  type Props = ModesTable

  protected val ModesTableMaker = TableMaker[ModeRow]

  import ModesTableMaker.syntax._

  protected val ModesTableComponent = new SUITable(ModesTableMaker)

  val disperserDisplay: Display[ModeDisperser] = Display.byShortName {
    case ModeDisperser.NoDisperser      => "-"
    case ModeDisperser.SomeDisperser(t) => t
  }

  def column[V](id: String, accessor: ModeRow => V) =
    ModesTableMaker
      .Column(id, accessor)
      .setHeader(columnNames(id))

  private val columnNames: Map[String, String] = Map("instrument" -> " ", "disperser" -> "")

  val columns =
    List(
      column("instrument", ModeRow.instrument.get)
        .setCell(_.value.shortName)
        .setWidth(30),
      column("disperser", ModeRow.disperser.get)
        .setCell(d => disperserDisplay.shortName(d.value))
        .setWidth(30)
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
  ) extends ReactProps[ModesTable](ModesTable.component)

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
          headerCell = (col: ModesTableMaker.ColumnType) =>
            TableHeaderCell(clazz = ExploreStyles.Sticky |+| ExploreStyles.ModesHeader)(
              ^.textTransform.capitalize,
              ^.whiteSpace.nowrap,
              col.id.toString
            )
        )(tableInstance)
      )
    }

}
