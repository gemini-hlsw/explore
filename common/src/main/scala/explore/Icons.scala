// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.components.ui.ExploreStyles
import react.fa.FAIcon
import react.fa.FontAwesomeIcon
import react.fa.IconLibrary

import scala.annotation.nowarn
import scala.scalajs.js
import scala.scalajs.js.annotation._

@nowarn
object Icons {
  @js.native
  @JSImport("@fortawesome/pro-duotone-svg-icons", "faCogs")
  val faCogs: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faUndo")
  val faUndo: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faRedo")
  val faRedo: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-light-svg-icons", "faBars")
  val faBars: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faTrashAlt")
  val faTrash: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faCrosshairs")
  val faCrosshairs: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-light-svg-icons", "faBullseye")
  val faBullseye: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faPlus")
  val faPlus: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faEdit")
  val faEdit: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faSearch")
  val faSearch: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faBan")
  val faBan: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faMousePointer")
  val faMousePointer: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faSignOutAlt")
  val faSignOutAlt: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faInfoCircle")
  val faInfoCircle: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-light-svg-icons", "faClipboard")
  val faClipboard: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-light-svg-icons", "faClipboardCheck")
  val faClipboardCheck: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faTimes")
  val faTimes: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faUserAstronaut")
  val faUserAstronaut: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faExclamationTriangle")
  val faExclamationTriangle: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faSkullCrossbones")
  val faSkullCrossbones: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faCheck")
  val faCheck: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faSort")
  val faSort: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faSortUp")
  val faSortUp: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faSortDown")
  val faSortDown: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faChevronRight")
  val faChevronRight: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-light-svg-icons", "faChevronRight")
  val faChevronRightLight: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faChevronLeft")
  val faChevronLeft: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-light-svg-icons", "faChevronLeft")
  val faChevronLeftLight: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faChevronDown")
  val faChevronDown: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-regular-svg-icons", "faChevronDoubleUp")
  val faChevronDoubleUp: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-regular-svg-icons", "faChevronDoubleDown")
  val faChevronDoubleDown: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-light-svg-icons", "faListAlt")
  val faListAlt: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-light-svg-icons", "faCompress")
  val faCompress: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-light-svg-icons", "faExpand")
  val faExpand: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faStar")
  val faStar: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faSpinner")
  val faSpinner: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faSun")
  val faSunBright: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faExclamationTriangle")
  val faTriangleSolid: FAIcon = js.native

  // This is tedious but lets us do proper tree-shaking
  IconLibrary.add(
    faCogs,
    faBars,
    faUndo,
    faRedo,
    faPlus,
    faTrash,
    faBullseye,
    faCrosshairs,
    faEdit,
    faSearch,
    faBan,
    faMousePointer,
    faSignOutAlt,
    faInfoCircle,
    faClipboard,
    faClipboardCheck,
    faTimes,
    faUserAstronaut,
    faExclamationTriangle,
    faSkullCrossbones,
    faCheck,
    faSort,
    faSortUp,
    faSortDown,
    faChevronRight,
    faChevronRightLight,
    faChevronLeft,
    faChevronLeftLight,
    faChevronDown,
    faChevronDoubleUp,
    faChevronDoubleDown,
    faListAlt,
    faCompress,
    faExpand,
    faStar,
    faSpinner,
    faSunBright,
    faTriangleSolid
  )

  val Bars                = FontAwesomeIcon(faBars)
  val Cogs                = FontAwesomeIcon(faCogs)
  val Undo                = FontAwesomeIcon(faUndo)
  val Redo                = FontAwesomeIcon(faRedo)
  val New                 = FontAwesomeIcon(faPlus)
  val Trash               = FontAwesomeIcon(faTrash).clazz(ExploreStyles.TrashIcon)
  val Bullseye            = FontAwesomeIcon(faBullseye)
  val Crosshairs          = FontAwesomeIcon(faCrosshairs)
  val Edit                = FontAwesomeIcon(faEdit)
  val Search              = FontAwesomeIcon(faSearch)
  val ChevronRight        = FontAwesomeIcon(faChevronRight)
  val ChevronRightLight   = FontAwesomeIcon(faChevronRightLight)
  val ChevronLeft         = FontAwesomeIcon(faChevronLeft)
  val ChevronLeftLight    = FontAwesomeIcon(faChevronLeftLight)
  val ChevronDown         = FontAwesomeIcon(faChevronDown)
  val ChevronDoubleUp     = FontAwesomeIcon(faChevronDoubleUp)
  val ChevronDoubleDown   = FontAwesomeIcon(faChevronDoubleDown)
  val Ban                 = FontAwesomeIcon(faBan)
  val Sort                = FontAwesomeIcon(faSort)
  val SortDown            = FontAwesomeIcon(faSortDown)
  val SortUp              = FontAwesomeIcon(faSortUp)
  val ExclamationTriangle = FontAwesomeIcon(faExclamationTriangle)
  val UserAstronaut       = FontAwesomeIcon(faUserAstronaut)
  val Logout              = FontAwesomeIcon(faSignOutAlt)
  val SkullCrossBones     = FontAwesomeIcon(faSkullCrossbones)
  val Info                = FontAwesomeIcon(faInfoCircle)
  val Clipboard           = FontAwesomeIcon(faClipboard)
  val ClipboardCheck      = FontAwesomeIcon(faClipboardCheck)
  val Close               = FontAwesomeIcon(faTimes)
  val MousePointer        = FontAwesomeIcon(faMousePointer)
  val Minimize            = FontAwesomeIcon(faCompress)
  val Maximize            = FontAwesomeIcon(faExpand)
  val Checkmark           = FontAwesomeIcon(faCheck)
  val ListIcon            = FontAwesomeIcon(faListAlt)
  val Star                = FontAwesomeIcon(faStar)
  val Spinner             = FontAwesomeIcon(faSpinner)
  val SunBright           = FontAwesomeIcon(faSunBright)
  val TriangleSolid       = FontAwesomeIcon(faTriangleSolid)
}
