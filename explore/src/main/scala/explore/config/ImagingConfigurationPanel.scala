// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.implicits._
import coulomb.Quantity
import coulomb.ops.algebra.spire.all.given
import coulomb.policy.spire.standard.given
import crystal.react.View
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import explore.components.HelpIcon
import explore.components.InputWithUnits
import explore.components.ui.ExploreStyles
import explore.model.AvailableFilter
import explore.model.ImagingConfigurationOptions
import explore.model.enums.ImagingCapabilities
import explore.model.formats._
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enums.FilterType
import lucuma.core.math.units._
import lucuma.core.util.Display
import lucuma.core.validation._
import lucuma.refined.*
import lucuma.ui.forms.EnumViewOptionalSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.syntax.all.given
import react.common.Css
import react.common.ReactFnProps
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

  implicit val capabDisplay: Display[ImagingCapabilities] = Display.by(_.label, _.label)

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

  def formatCentral(r: Quantity[Rational, Nanometer]): String =
    if (r.value > 1000)
      f"${r.toValue[Double].toUnit[Micrometer].value}%.3f μm"
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
    ScalaFnComponent[Props] { p =>
      val filters       = p.options.zoom(ImagingConfigurationOptions.filters)
      val fov           = p.options.zoom(ImagingConfigurationOptions.fov)
      val signalToNoise = p.options.zoom(ImagingConfigurationOptions.signalToNoise)
      val capabilities  = p.options.zoom(ImagingConfigurationOptions.capabilities)

      ReactFragment(
        <.label("Filter", HelpIcon("configuration/filter.md".refined), ExploreStyles.SkipToNext),
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
                (r: Any) match {
                  case v: js.Array[?] =>
                    filters.set(valuesToFilters(v.collect { case s: String => s }))
                  case _              => Callback.empty
                }
              )
              .getOrEmpty
        ),
        <.label("Field of View",
                HelpIcon("configuration/fov.md".refined),
                ExploreStyles.SkipToNext
        ),
        InputWithUnits(
          id = "configuration-fov".refined,
          clazz = Css.Empty,
          inline = true,
          value = fov,
          units = "arcsec",
          validFormat = InputValidWedge.fromFormat(formatArcsec).optional,
          changeAuditor = ChangeAuditor.fromFormat(formatArcsec).optional,
          disabled = false
        ),
        <.label("S / N",
                HelpIcon("configuration/signal_to_noise.md".refined),
                ExploreStyles.SkipToNext
        ),
        FormInputEV(
          id = "signal-to-noise".refined,
          value = signalToNoise,
          validFormat = InputValidSplitEpi.posBigDecimal.optional,
          changeAuditor = ChangeAuditor.posBigDecimal().optional
        ),
        <.label(
          "Capabilities",
          HelpIcon("configuration/capabilities.md".refined),
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
