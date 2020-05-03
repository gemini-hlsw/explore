// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.typelevel.discipline
package munit

import _root_.munit.Location
import _root_.munit.ScalaCheckSuite

trait Discipline extends ScalaCheckSuite {

  def checkAll(name: String, ruleSet: Laws#RuleSet)(implicit
    loc:             Location
  ) =
    ruleSet.all.properties.toList.foreach {
      case (id, prop) =>
        property(s"$name: $id") {
          prop
        }
    }

}
