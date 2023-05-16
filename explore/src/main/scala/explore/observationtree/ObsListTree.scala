// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import explore.data.tree.KeyedIndexedTree
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.table.*
import lucuma.typed.primereact.treeTreeMod.TreeNodeTemplateOptions
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import org.scalajs.dom.HTMLDivElement
import react.common.ReactFnProps
import react.primereact.Tree

import scala.scalajs.js

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
