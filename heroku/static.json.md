(This file exists since we can't add comments to a JSON file.)

The redirect directive uses an nginx hack to allow relative redirects as explained here: https://stackoverflow.com/a/39462409

This is necessary since by default, if we did `"url": "/conf/${ENVIRONMENT}.conf.json"`, nginx forces the protocol in the redirect location to `http`, and browsers refuse to load it if the app was retrieved by `https` (even if the `http` request redirects later to `https`).

Alternatively, we could write something like ``"url": "https://$host/conf/${ENVIRONMENT}.conf.json"` to force the protocol to `https` always.
