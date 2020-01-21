In order to deploy to Heroku, the Heroku app must be configured to provision `node` besides the `Scala` environment.

This can be done similarly to what is described in https://devcenter.heroku.com/articles/using-node-js-to-perform-javascript-optimization-for-play-and-scala-applications, namely:

```
heroku buildpacks:clear
heroku buildpacks:add heroku/nodejs
heroku buildpacks:add heroku/scala
```