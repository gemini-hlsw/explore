// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.syntax

import explore.model.syntax.all.*
import lucuma.core.util.TimeSpan

import java.time.Duration

class SyntaxSuite extends munit.FunSuite:

  test("toHoursMinutes should format a TimeSpan"):
    assertEquals(TimeSpan.Zero.toHoursMinutes, "0mins")
    assertEquals(TimeSpan.unsafeFromDuration(Duration.ofHours(1)).toHoursMinutes, "1hrs")
    assertEquals(TimeSpan.unsafeFromDuration(Duration.ofMinutes(30)).toHoursMinutes, "30mins")
    assertEquals(TimeSpan.unsafeFromDuration(Duration.ofHours(1).plusMinutes(30)).toHoursMinutes,
                 "1hrs 30mins"
    )

  test("toHoursMinutes should round to the nearest minute"):
    assertEquals(TimeSpan.unsafeFromDuration(Duration.ofMinutes(30).plusSeconds(30)).toHoursMinutes,
                 "31mins"
    )
    assertEquals(TimeSpan.unsafeFromDuration(Duration.ofMinutes(30).plusSeconds(29)).toHoursMinutes,
                 "30mins"
    )
    assertEquals(TimeSpan
                   .unsafeFromDuration(Duration.ofHours(1).plusMinutes(30).plusSeconds(30))
                   .toHoursMinutes,
                 "1hrs 31mins"
    )
