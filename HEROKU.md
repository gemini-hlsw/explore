In order to deploy a Scala.js app to Heroku, the Heroku app must be configured to provision `node` besides the `Scala` environment.

This can be done similarly to what is described in https://devcenter.heroku.com/articles/using-node-js-to-perform-javascript-optimization-for-play-and-scala-applications.

Furthermore, we will add `nginx` to serve the app and its static files, as well as define it as reverse proxy. For this we use https://github.com/heroku/heroku-buildpack-static, similarly to what is described in https://m.alphasights.com/using-nginx-on-heroku-to-serve-single-page-apps-and-avoid-cors-5d013b171a45. For development, we configure a similar proxy in Webpack Dev Server.

The buildpacks are configured in `app.json`. If you want to avoid setting them up manually, you can have Heroku pick it by navigating to:

https://heroku.com/deploy?template=https://github.com/gemini-hlsw/explore/tree/master (or your fork's and desired branch GitHub address)

(`app.json` is also picked up for Review Apps when using Heroku CI).

The resulting Heroku app won't be connected to the GitHub repo though. That has to be done manually via web: https://dashboard.heroku.com/apps/<<app-name>>/deploy/github


*BTW...*

* `JVM` version is in `/system.properties`.
* `node` version is in `/package.json`.
* To perform a clean build, set environment variable `SBT_CLEAN=true` and then remove it after the build (otherwise all subsequent builds will do a clean build).
* To clean the build cache: https://help.heroku.com/18PI5RSY/how-do-i-clear-the-build-cache