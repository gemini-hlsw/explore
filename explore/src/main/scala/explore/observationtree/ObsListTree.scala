// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import explore.data.tree.KeyedIndexedTree
import react.common.ReactFnProps
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import cats.syntax.all.*
import explore.data.tree.Node
import scala.scalajs.js
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import js.JSConverters._
import lucuma.react.table.Expandable
import lucuma.ui.table.*
import explore.model.reusability.given
import lucuma.ui.reusability.given
import lucuma.react.table.*
import lucuma.react.table.ColumnDef
import org.scalajs.dom.HTMLDivElement
import explore.observationtree.ObsNode.given
import explore.components.ui.ExploreStyles
import react.primereact.Tree
import lucuma.typed.primereact.treeTreeMod.TreeNodeTemplateOptions

case class ObsListTree[A](
  obsList:    Seq[Tree.Node[A]],
  renderItem: (A, TreeNodeTemplateOptions) => VdomNode
) extends ReactFnProps(ObsListTree.component)

object ObsListTree:
  private type Props[A] = ObsListTree[A]

  private val ColDef = ColumnDef[Expandable[ObsNode]]

  private def component[A] =
    ScalaFnComponent
      .withHooks[Props[A]]
      .render(props =>
        <.div(
          Tree(
            props.obsList,
            nodeTemplate = props.renderItem
          )
        )
      )
