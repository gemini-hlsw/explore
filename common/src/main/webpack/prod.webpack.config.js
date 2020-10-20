const path = require("path");
const Merge = require("webpack-merge");
const parts = require("./webpack.parts");
const ScalaJSConfig = require("./scalajs.webpack.config");
const FaviconsWebpackPlugin = require("favicons-webpack-plugin");

const HtmlWebpackPlugin = require("html-webpack-plugin");

const isDev = false;
const ci = process.env.CI === 'true'; // When on CI don't add hashes

const Web = Merge(
  ScalaJSConfig,
  parts.noNode, // We will have to use this in Scala 1.x, but doesn't work with 0.6
  parts.resolve,
  parts.resolveSemanticUI,
  parts.resourceModules,
  parts.extractCSS({
    devMode: false,
    useLess: ["css-loader", parts.autoprefix(), parts.lessLoader(isDev)], // Order is very important: css, post-css, (le|sa)ss
    useSass: ["css-loader", parts.autoprefix(), parts.sassLoader(isDev)],
    ci: ci,
  }),
  parts.minifyJavaScript(),
  parts.minifyCSS({
    options: {
      safe: true,
      mergeLonghand: false, // Required to avoid merges of border properties that are unsafe
      discardComments: { removeAll: true },
      autoprefixer: { disable: true }, // Otherwise this conflicts with post-css autoprefixer
    },
  }),
  parts.extraAssets,
  parts.fontAssets,
  {
    mode: "production",
    entry: {
      "explore-opt": path.resolve(parts.localResourcesDir, "./prod.js"), // If name is the same as scala.js output, it's not emitted twice.
    },
    output: {
      path: parts.stageDir,
      filename: ci ? "[name].js" : "[name].[chunkhash].js",
      publicPath: "/", // Required to make url navigation work
    },
    plugins: [
      new HtmlWebpackPlugin({
        title: "Explore",
        filename: "index.html",
      }),
      new FaviconsWebpackPlugin({
        logo: path.resolve(parts.resourcesDir, "images/logo.png"),
        persistentCache: false,
      }),
    ],
    node: {
      fs: "empty"
    }
  },
);

module.exports = Web;
