# Explore

## Launch on local development

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
