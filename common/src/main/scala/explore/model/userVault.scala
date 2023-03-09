// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import clue.js.FetchJSRequest

object userVault:
  extension (vault: UserVault)
    def authorizationHeader: String = s"Bearer ${vault.token.value}"

    def addAuthorizationHeader(request: FetchJSRequest): FetchJSRequest =
      // DOM Headers are mutable
      request.headers.set("Authorization", authorizationHeader)
      request
