# Explore

[![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-blue.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)

## Launch on local development

We are now using FontAwesome Pro which requires a license. To build the app locally request a TOKEN
from the admins and you need to setup an env variable containing it like

```bash
export FONTAWESOME_NPM_AUTH_TOKEN=...
```

or

```fish
set -x FONTAWESOME_NPM_AUTH_TOKEN ...
```

For auth to work you need your app to run on the `lucuma.xyz` domain, the simplest way
is to setup `/etc/host` adding an alias for `localhost`

```
127.0.0.1   localhost local.lucuma.xyz
```

Alternatively if you run a home DNS server you can provide a local alias. This has the benefit
of opening testing to any device in the network

First you need to build a copy of the fastLinkJS version of explore

```
sbt explore/fastLinkJS
```

To launch explore you can use vite development server going to the `explore` dir and
calling the command `npx vite`

You may need to update your node modules via `npm install`

With that you can open the app at:
http://local.lucuma.xyz:8080/

## Test full deployment

In same cases you may want to test locally how the app looks without deploying. In that case you need to:

- Build a full link version of explore

```
sbt explore/fullLinkJS
```

- Build it with vite and launch

```
npx vite build && npx vite preview
```

As before you can now see the app locally but in a different port
http://local.lucuma.xyz:5000/

## Testing PWA locally

Besides doing a full deployment you need to comment out the condition about local.lucuma.xyz on main.jsx

Normally you want to test changes thus you need to do even a tiny change and build again to trigger an update prompt

## Bundle sizes

You can check the evolution of bundle sizes [here](https://app.bundlemon.dev/projects/61a698e5de59ab000954f941/reports?branch=master&resolution=all)
