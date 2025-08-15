// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.model.formats
import lucuma.core.math.Epoch
import munit.FunSuite

import java.time.LocalDateTime

class EpochFormattingSuite extends FunSuite:

  test("January 1st test"):
    val testCases = List(
      (2012, "J2012"),
      (2013, "J2013.002"),
      (2014, "J2014.001"),
      (2015, "J2015"),
      (2016, "J2016"),
      (2017, "J2017.002"),
      (2018, "J2018.001"),
      (2019, "J2019"),
      (2020, "J2020"),
      (2021, "J2021.002"),
      (2022, "J2022.001"),
      (2023, "J2023"),
      (2024, "J2024"),
      (2025, "J2025.002"),
      (2026, "J2026.001"),
      (2027, "J2027")
    )

    testCases.foreach: (year, originalExpected) =>
      val ldt   = LocalDateTime.of(year, 1, 1, 0, 0, 0)
      val epoch = Epoch.Julian.fromLocalDateTime(ldt).get

      val format   = formats.formatEpochWithScheme(epoch)
      val expected = s"J$year.00"

      assertEquals(format, expected)
      assertNotEquals(format, originalExpected)

      val parsedEpoch = Epoch.fromStringNoScheme.getOption(s"$year.00")
      assert(parsedEpoch.isDefined)

  test("December 31st tests"):
    val endOfYearDates = List(
      (LocalDateTime.of(2015, 12, 31, 23, 59, 59), "J2015.99"),
      (LocalDateTime.of(2016, 12, 31, 23, 59, 59), "J2016.99"),
      (LocalDateTime.of(2023, 12, 31, 23, 59, 59), "J2023.99"),
      (LocalDateTime.of(2024, 12, 31, 23, 59, 59), "J2024.99"),
      (LocalDateTime.of(2025, 12, 31, 23, 59, 59), "J2025.99")
    )

    endOfYearDates.foreach: (ldt, expected) =>
      val epoch     = Epoch.Julian.fromLocalDateTime(ldt).get
      val formatted = formats.formatEpochWithScheme(epoch)

      assertEquals(formatted, expected)

  test("Edge cases around year boundaries"):
    val boundaryDates = List(
      (LocalDateTime.of(2024, 12, 31, 23, 59, 59), "J2024.99"),
      (LocalDateTime.of(2025, 1, 1, 0, 0, 0), "J2025.00"),
      (LocalDateTime.of(2024, 12, 31, 12, 0, 0), "J2024.99"),
      (LocalDateTime.of(2025, 1, 1, 12, 0, 0), "J2025.00"),
      (LocalDateTime.of(2025, 1, 2, 0, 0, 0), "J2025.00")
    )

    boundaryDates.foreach: (ldt, expected) =>
      val epoch     = Epoch.Julian.fromLocalDateTime(ldt).get
      val formatted = formats.formatEpochWithScheme(epoch)

      assertEquals(formatted, expected)
