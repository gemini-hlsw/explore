// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.implicits._
import coulomb.Quantity
import crystal.react.View
import explore.model.reusability._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.AvailableFilter
import explore.model.ImagingConfigurationOptions
import explore.model.enum.ImagingCapabilities
import explore.model.formats._
import explore.targeteditor.InputWithUnits
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.FilterType
import lucuma.core.math.units._
import lucuma.core.util.Display
import lucuma.ui.forms.EnumViewOptionalSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import react.common._
import react.semanticui.collections.menu.MenuHeader
import react.semanticui.modules.dropdown._
import spire.math.Rational

import scala.collection.immutable.SortedSet
import scala.scalajs.js
import scala.scalajs.js.JSConverters._

final case class ImagingConfigurationPanel(
  options: View[ImagingConfigurationOptions]
) extends ReactFnProps[ImagingConfigurationPanel](ImagingConfigurationPanel.component)

object ImagingConfigurationPanel {
  type Props         = ImagingConfigurationPanel
  type SectionHeader = String

  implicit val capabDisplay: Display[ImagingCapabilities]             = Display.by(_.label, _.label)
  implicit val optionsReuse: Reusability[ImagingConfigurationOptions] = Reusability.derive
  implicit val avalableFiltersReuse: Reusability[AvailableFilter]     = Reusability.by(_.tag)
  // implicit val filtersSetReuse: Reusability[SortedSet[AvailableFilter]] = Reusability.by(_.toList)
  implicit val propsReuse: Reusability[Props]                         = Reusability.derive

  val byFilterType = ImagingConfigurationOptions.availableOptions.groupBy(_.filterType)
  val broadBand    = byFilterType.getOrElse(FilterType.BroadBand, Nil).sortBy(_.centralWavelength)
  val narrowBand   = byFilterType.getOrElse(FilterType.NarrowBand, Nil).sortBy(_.centralWavelength)
  val combination  = byFilterType.getOrElse(FilterType.Combination, Nil).sortBy(_.centralWavelength)

  def valuesToFilters(v: js.Array[String]): SortedSet[AvailableFilter] =
    SortedSet(
      v.map { t =>
        ImagingConfigurationOptions.availableOptions.find(_.tag === t)
      }.collect { case Some(x) =>
        x
      }.toList: _*
    )

  def formatCentral(r: Quantity[Int, Nanometer]): String =
    if (r.value > 1000)
      f"${r.to[Rational, Micrometer].value.toDouble}%.3f μm"
    else
      s"${r.value.toInt} nm"

  def formatRange(r: Quantity[Int, Nanometer]): String =
    s"${r.value.toInt} nm"

  def filterItem(f: Either[SectionHeader, AvailableFilter]) =
    DropdownItem(
      value = f.fold(identity, _.tag),
      text = f.fold(identity, _.shortName),
      content = f match {
        case Left(f)  =>
          MenuHeader(f): VdomNode
        case Right(f) =>
          <.div(
            ExploreStyles.ConfigurationFilterItem,
            <.span(f.shortName),
            <.span(formatCentral(f.centralWavelength.nanometer)),
            <.span(f.range.map(formatRange))
          )
      },
      selected = false
    )

  val options: List[Option[Either[SectionHeader, AvailableFilter]]] =
    "Broad band".asLeft.some ::
      broadBand.map(f => f.asRight.some) :::
      ("Narrow band".asLeft.some ::
        narrowBand.map(f => f.asRight.some)) :::
      ("Combination".asLeft.some ::
        combination.map(f => f.asRight.some))

  protected val component =
    ScalaFnComponent
      .withReuse[Props] { p =>
        val filters       = p.options.zoom(ImagingConfigurationOptions.filters)
        val fov           = p.options.zoom(ImagingConfigurationOptions.fov)
        val signalToNoise = p.options.zoom(ImagingConfigurationOptions.signalToNoise)
        val capabilities  = p.options.zoom(ImagingConfigurationOptions.capabilities)

        ReactFragment(
          <.label("Filter", HelpIcon("configuration/filter.md"), ExploreStyles.SkipToNext),
          Dropdown(
            placeholder = "Filters",
            clazz = ExploreStyles.ConfigurationFilter,
            selection = true,
            multiple = true,
            search = true,
            value = filters.get.toList.map(_.tag).toJSArray,
            options = options.collect { case Some(x) => filterItem(x) },
            onChange = (ddp: Dropdown.DropdownProps) =>
              ddp.value.toOption
                .map(r =>
                  ((r: Any) match {
                    case v: js.Array[_] =>
                      filters.set(valuesToFilters(v.collect { case s: String => s }))
                    case _              => Callback.empty
                  })
                )
                .getOrEmpty
          ),
          <.label("Field of View", HelpIcon("configuration/fov.md"), ExploreStyles.SkipToNext),
          InputWithUnits(
            id = "configuration-fov",
            clazz = Css.Empty,
            inline = true,
            value = fov,
            units = "arcsec",
            validFormat = ValidFormatInput.fromFormat(formatArcsec).optional,
            changeAuditor = ChangeAuditor.fromFormat(formatArcsec).optional,
            disabled = false
          ),
          <.label("S / N", HelpIcon("configuration/signal_to_noise.md"), ExploreStyles.SkipToNext),
          FormInputEV(
            id = "signal-to-noise",
            value = signalToNoise,
            validFormat = ValidFormatInput.forPosBigDecimal().optional,
            changeAuditor = ChangeAuditor.posBigDecimal().optional
          ),
          <.label("Capabilities",
                  HelpIcon("configuration/capabilities.md"),
                  ExploreStyles.SkipToNext
          ),
          EnumViewOptionalSelect(
            id = "imaging-capabilities",
            clazz = ExploreStyles.ConfigurationCapabilities,
            clearable = true,
            upward = true,
            placeholder = "Extra capablities",
            value = capabilities
          )
        )
      }
}
