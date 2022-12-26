// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.implicits.*
import coulomb.Quantity
import coulomb.ops.algebra.spire.all.given
import coulomb.policy.spire.standard.given
import crystal.react.View
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AvailableFilter
import explore.model.ImagingConfigurationOptions
import explore.model.enums.ImagingCapabilities
import explore.model.formats.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.FilterType
import lucuma.core.math.units.*
import lucuma.core.util.Display
import lucuma.core.validation.*
import lucuma.refined.*
import lucuma.ui.forms.EnumViewOptionalSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.FormEnumDropdownOptionalView
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.LucumaStyles
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import react.common.Css
import react.common.ReactFnProps
import react.primereact.PrimeStyles
import react.semanticui.collections.menu.MenuHeader
import react.semanticui.modules.dropdown.*
import spire.math.Rational

import scala.collection.immutable.SortedSet
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

case class ImagingConfigurationPanel(
  options: View[ImagingConfigurationOptions]
) extends ReactFnProps(ImagingConfigurationPanel.component)

object ImagingConfigurationPanel {
  private type Props         = ImagingConfigurationPanel
  private type SectionHeader = String

  private given Display[ImagingCapabilities] = Display.by(_.label, _.label)

  private val byFilterType = ImagingConfigurationOptions.availableOptions.groupBy(_.filterType)
  private val broadBand    =
    byFilterType.getOrElse(FilterType.BroadBand, Nil).sortBy(_.centralWavelength)
  private val narrowBand   =
    byFilterType.getOrElse(FilterType.NarrowBand, Nil).sortBy(_.centralWavelength)
  private val combination  =
    byFilterType.getOrElse(FilterType.Combination, Nil).sortBy(_.centralWavelength)

  private def valuesToFilters(v: js.Array[String]): SortedSet[AvailableFilter] =
    SortedSet(
      v.map { t =>
        ImagingConfigurationOptions.availableOptions.find(_.tag === t)
      }.collect { case Some(x) =>
        x
      }.toList: _*
    )

  private def formatCentral(r: Quantity[Rational, Nanometer]): String =
    if (r.value > 1000)
      f"${r.toValue[Double].toUnit[Micrometer].value}%.3f μm"
    else
      s"${r.value.toInt} nm"

  private def formatRange(r: Quantity[Int, Nanometer]): String =
    s"${r.value.toInt} nm"

  private def filterItem(f: Either[SectionHeader, AvailableFilter]) =
    DropdownItem(
      value = f.fold(identity, _.tag),
      text = f.fold(identity, _.shortName),
      content = f match {
        case Left(f)  =>
          MenuHeader(content = f): VdomNode
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

  private val options: List[Option[Either[SectionHeader, AvailableFilter]]] =
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
        FormInputTextView(
          id = "configuration-fov".refined,
          value = fov,
          label = ReactFragment("Field of View", HelpIcon("configuration/fov.md".refined)),
          postAddons = List("arcsec"),
          validFormat = InputValidWedge.fromFormat(formatArcsec).optional,
          changeAuditor = ChangeAuditor.fromFormat(formatArcsec).optional
        ),
        FormInputTextView(
          id = "signal-to-noise".refined,
          value = signalToNoise,
          label = ReactFragment("S / N", HelpIcon("configuration/signal_to_noise.md".refined)),
          validFormat = InputValidSplitEpi.posBigDecimal.optional,
          changeAuditor = ChangeAuditor.posBigDecimal().optional
        ),
        FormEnumDropdownOptionalView(
          id = "imaging-capabilities".refined,
          label = ReactFragment(
            "Capabilities",
            HelpIcon("configuration/capabilities.md".refined)
          ),
          placeholder = "Extra capablities",
          value = capabilities
        )
      )
    }
}
