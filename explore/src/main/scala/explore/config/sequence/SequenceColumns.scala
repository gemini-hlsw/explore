// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.syntax.all.*
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.table.*
import lucuma.ui.sequence.SequenceRow
import lucuma.ui.sequence.SequenceRowFormatters.*
import lucuma.ui.syntax.all.given

object SequenceColumns:
  val StepTypeColumnId: ColumnId   = ColumnId("stepType")
  val ExposureColumnId: ColumnId   = ColumnId("exposure")
  val GuideColumnId: ColumnId      = ColumnId("guide")
  val PColumnId: ColumnId          = ColumnId("p")
  val QColumnId: ColumnId          = ColumnId("q")
  val WavelengthColumnId: ColumnId = ColumnId("lambda")
  val FPUColumnId: ColumnId        = ColumnId("fpu")
  val GratingColumnId: ColumnId    = ColumnId("grating")
  val FilterColumnId: ColumnId     = ColumnId("filter")
  val XBinColumnId: ColumnId       = ColumnId("xbin")
  val YBinColumnId: ColumnId       = ColumnId("Ybin")
  val ROIColumnId: ColumnId        = ColumnId("roi")
  val SNColumnId: ColumnId         = ColumnId("sn")

  private def rightAligned(value: Any) =
    <.div(^.textAlign.right)(value.toString)

  def gmosColumns[D, T, R <: SequenceRow[D]](
    colDef:  ColumnDef.Applied[T],
    getStep: T => Option[R]
  ): List[ColumnDef[T, ?]] =
    List(
      colDef(
        StepTypeColumnId,
        getStep(_).flatMap(_.stepType),
        header = "Type",
        cell = _.value.map(_.toString)
      ),
      colDef(
        ExposureColumnId,
        getStep(_).flatMap(_.exposureTime),
        header = _ => rightAligned("Exp (sec)"),
        cell = c =>
          (c.value, getStep(c.row.original).flatMap(_.instrument)).mapN: (e, i) =>
            rightAligned(FormatExposureTime(i)(e))
      ),
      colDef(
        GuideColumnId,
        getStep(_).map(_.hasGuiding),
        header = "",
        cell = _.value
          .filter(identity) // Only render on Some(true)
          .map(_ => Icons.Crosshairs.withClass(ExploreStyles.StepGuided))
      ),
      colDef(
        PColumnId,
        getStep(_).flatMap(_.p),
        header = _ => rightAligned("p"),
        cell = _.value.map(rightAligned.compose(FormatOffsetP))
      ),
      colDef(
        QColumnId,
        getStep(_).flatMap(_.q),
        header = _ => rightAligned("q"),
        cell = _.value.map(rightAligned.compose(FormatOffsetQ))
      ),
      colDef(
        WavelengthColumnId,
        getStep(_).flatMap(_.wavelength),
        header = _ => rightAligned("λ (nm)"),
        cell = _.value.map(rightAligned.compose(FormatWavelength))
      ),
      colDef(
        FPUColumnId,
        getStep(_).flatMap(_.fpuName),
        header = _ => rightAligned("FPU"),
        cell = _.value.map(rightAligned)
      ),
      colDef(
        GratingColumnId,
        getStep(_).flatMap(_.gratingName),
        header = "Grating",
        cell = _.value.orEmpty
      ),
      colDef(
        FilterColumnId,
        getStep(_).flatMap(_.filterName),
        header = "Filter",
        cell = _.value.orEmpty
      ),
      colDef(
        XBinColumnId,
        getStep(_).flatMap(_.readoutXBin),
        header = _ => rightAligned("Xbin"),
        cell = cell => rightAligned(cell.value.orEmpty)
      ),
      colDef(
        YBinColumnId,
        getStep(_).flatMap(_.readoutYBin),
        header = _ => rightAligned("Ybin"),
        cell = cell => rightAligned(cell.value.orEmpty)
      ),
      colDef(ROIColumnId, getStep(_).flatMap(_.roi), header = "ROI", cell = _.value.orEmpty),
      colDef(SNColumnId, _ => "", header = "S/N")
    )
