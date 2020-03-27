In order to deploy a Scala.js app to Heroku, the Heroku app must be configured to provision `node` besides the `Scala` environment.

This can be done similarly to what is described in https://devcenter.heroku.com/articles/using-node-js-to-perform-javascript-optimization-for-play-and-scala-applications.

Furthermore, we will add `nginx` to serve the app and its static files, as well as define it as reverse proxy. For this we use https://github.com/heroku/heroku-buildpack-static, similarly to what is described in https://m.alphasights.com/using-nginx-on-heroku-to-serve-single-page-apps-and-avoid-cors-5d013b171a45. For development, we configure a similar proxy in Webpack Dev Server.

However, we configure the buildpack stack in `app.json` instead of doing it manually as the tutorials show.