// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// package explore.config

// import cats.implicits.*
// import coulomb.Quantity
// import crystal.react.View
// import eu.timepit.refined.auto.*
// import eu.timepit.refined.types.numeric.PosBigDecimal
// import explore.components.ui.ExploreStyles
// import explore.model.AvailableFilter
// import explore.model.ImagingConfigurationOptions
// import japgolly.scalajs.react.*
// import japgolly.scalajs.react.vdom.html_<^.*
// import lucuma.core.enums.FilterType
// import lucuma.core.math.units.*
// import lucuma.react.common.Css
// import lucuma.react.common.ReactFnProps
// import lucuma.react.primereact.MultiSelect
// import lucuma.react.primereact.SelectItem
// import lucuma.react.primereact.SelectItemGroup
// import lucuma.react.primereact.SelectItemGroups
// import lucuma.ui.primereact.LucumaPrimeStyles
// import lucuma.ui.syntax.all.given

// import scala.collection.immutable.SortedSet

// case class FiltersMultiSelect(
//   options:  View[ImagingConfigurationOptions],
//   readonly: Boolean
// ) extends ReactFnProps(FiltersMultiSelect.component)

// object FiltersMultiSelect extends ConfigurationFormats {
//   private type Props = FiltersMultiSelect

//   private val byFilterType = ImagingConfigurationOptions.availableOptions.groupBy(_.filterType)
//   private val broadBand    =
//     byFilterType.getOrElse(FilterType.BroadBand, Nil).sortBy(_.centralWavelength)
//   private val narrowBand   =
//     byFilterType.getOrElse(FilterType.NarrowBand, Nil).sortBy(_.centralWavelength)
//   private val combination  =
//     byFilterType.getOrElse(FilterType.Combination, Nil).sortBy(_.centralWavelength)

//   private def formatCentral(r: Quantity[PosBigDecimal, Micrometer]): String =
//     if (r.value > 1000)
//       f"${r.value.value.toDouble}%.3f μm"
//     else
//       s"${r.value.toInt} nm"

//   private def formatRange(r: Quantity[Int, Nanometer]): String =
//     s"${r.value.toInt} nm"

//   extension (filter: AvailableFilter)
//     def toSelectItem: SelectItem[AvailableFilter] =
//       SelectItem(value = filter, label = filter.shortName)

//   private val filterGroups: SelectItemGroups[AvailableFilter] = SelectItemGroups(groups =
//     List(
//       SelectItemGroup(label = "Broad Band", options = broadBand.map(_.toSelectItem)),
//       SelectItemGroup(label = "Narrow Band", options = narrowBand.map(_.toSelectItem)),
//       SelectItemGroup(label = "Combination", options = combination.map(_.toSelectItem))
//     )
//   )

//   protected val component =
//     ScalaFnComponent[Props] { p =>
//       val filters = p.options.zoom(ImagingConfigurationOptions.filters)

//       MultiSelect(
//         id = "filters",
//         value = filters.get.toList,
//         options = filterGroups,
//         clazz = LucumaPrimeStyles.FormField,
//         panelClass = ExploreStyles.ConfigurationFilter,
//         filter = true,
//         showSelectAll = false,
//         display = MultiSelect.Display.Chip,
//         onChange = fs => filters.set(SortedSet.from(fs)),
//         itemTemplate = si =>
//           <.div(
//             <.span(si.value.shortName),
//             <.span(formatCentral(si.value.centralWavelength.toMicrometers)),
//             <.span(si.value.range.map(formatRange))
//           )
//       )
//     }
// }
