# Heroku

## Serving

Since the runtime application consists only of static files, we use https://github.com/hone/heroku-buildpack-static to serve them.

Originally, we were running the build Heroku, which then served the output assets with said buildpack. (Description of this process is detailed below in case we resume it in the future).

Currently, we are _not_ performing builds in Heroku though. This is because Heroku always runs builds in a `performance-m` dyno, which provisions 2.5Gb of memory, which is not enough in our case. We are building in GitHub actions instead (which provide 7Gb of memory) and then deploying the result to Heroku (all this is in `ci.yml`). This works, but we lose the ability to deploy review apps, at least with the press of a button in the Heroku pipeline.

In this setup, none of these Heroku configuration files mentioned below are being used: `/system.properties`, `/package.json`, `/static.json` (there's one being used in `/explore/.../resources/static`). In `app.json`, the `environments/test` section is used since we still run CI in Heroku, but the rest of the file is ignored.

The following secrets need to be defined in GitHub: `HEROKU_API_KEY` and `HEROKU_APP_NAME`. The latter should name the first app in the pipeline. Promotion within the pipeline should work since it doesn't involve building again (the slug is copied).

## How we did things before... (And would like to go back to if Heroku ever provides more memory for builds)

In order to deploy a Scala.js app to Heroku, the Heroku app must be configured to provision `scala` and `node` environment.
The order is important. We want to first build on scala (doing a fullLinkJS on explore) and later a node build that will take the js built from Scala and `bundle` them. Note that `vite` doesn't really do bundling as understood on `webpack`

This can be done similarly to what is described in https://devcenter.heroku.com/articles/using-node-js-to-perform-javascript-optimization-for-play-and-scala-applications.

Furthermore, we will add `nginx` to serve the app and its static files, as well as define it as reverse proxy. For this we use https://github.com/heroku/heroku-buildpack-static, similarly to what is described in https://m.alphasights.com/using-nginx-on-heroku-to-serve-single-page-apps-and-avoid-cors-5d013b171a45. For development, we configure a similar proxy in Webpack Dev Server.

The buildpacks are configured in `app.json`. If you want to avoid setting them up manually, you can have Heroku pick it by navigating to:

https://heroku.com/deploy?template=https://github.com/gemini-hlsw/explore/tree/master (or your fork's and desired branch GitHub address)

(`app.json` is also picked up for Review Apps when using Heroku CI).

The resulting Heroku app won't be connected to the GitHub repo though. That has to be done manually via web: https://dashboard.heroku.com/apps/<<app-name>>/deploy/github

_BTW..._

- `JVM` version is in `/system.properties`.
- `node` version is in `/package.json`.
- To perform a clean build, set environment variable `SBT_CLEAN=true` and then remove it after the build (otherwise all subsequent builds will do a clean build).
- To clean the build cache: https://help.heroku.com/18PI5RSY/how-do-i-clear-the-build-cache
