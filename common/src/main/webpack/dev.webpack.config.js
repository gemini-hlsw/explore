const path = require("path");
const Webpack = require("webpack");
const Merge = require("webpack-merge");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const parts = require("./webpack.parts");
const ScalaJSConfig = require("./scalajs.webpack.config");

const isDev = true;
const isDevServer = process.argv.some((s) =>
  s.match(/webpack-dev-server\.js$/)
);

const Web = Merge(
  ScalaJSConfig,
  // parts.noNode, // We will have to use this in Scala 1.x, but doesn't work with 0.6
  parts.resolve,
  parts.resolveSemanticUI,
  parts.resourceModules,
  parts.extractCSS({
    devMode: true,
    useLess: ["css-loader", parts.lessLoader(isDev)],
    useSass: ["css-loader", parts.sassLoader(isDev)],
  }),
  parts.extraAssets,
  parts.fontAssets,
  {
    mode: "development",
    entry: {
      demo: [path.resolve(parts.localResourcesDir, "./dev.js")],
    },
    output: {
      publicPath: "/", // Required to make the url navigation work
    },
    module: {
      // Don't parse scala.js code. it's just too slow
      noParse: function (content) {
        return content.endsWith("-fastopt");
      },
    },
    // Custom dev server for the demo as we need a ws proxy
    devServer: {
      host: "0.0.0.0",
      hot: true,
      contentBase: [__dirname, parts.rootDir],
      disableHostCheck: true,
      historyApiFallback: {
        rewrites: [
          { from: /^\/conf.json$/, to: '/conf/development.conf.json' },
        ]
      }
    },
    plugins: [
      // Needed to enable HMR
      new Webpack.HotModuleReplacementPlugin(),
      new HtmlWebpackPlugin({
        filename: "index.html",
        title: "Explore",
        chunks: ["demo"],
        meta: { "color-scheme": "dark" },
      }),
    ],
  }
);

// Enable status bar to display on the page when webpack is reloading
if (isDevServer) {
  Web.entry.demo.push("webpack-dev-server-status-bar");
}

module.exports = Web;
