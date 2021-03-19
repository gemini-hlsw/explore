import reactRefresh from "@vitejs/plugin-react-refresh";
// import pluginRewriteConf from "./rewriteconf";
import path from "path";
import fs from "fs";
import ViteFonts from "vite-plugin-fonts";

const fontImport = ViteFonts({
  google: {
    families: [
      {
        name: "Lato",
        styles: "ital,wght@0,400;0,700;1,400;1,700",
      },
    ],
  },
});

// https://vitejs.dev/config/
export default ({ command, mode }) => {
  const sjs =
    mode == "production"
      ? path.resolve(__dirname, "target/scala-2.13/explore-opt")
      : path.resolve(__dirname, "target/scala-2.13/explore-fastopt/");
  const common = path.resolve(__dirname, "../common/");
  const webappCommon = path.resolve(common, "src/main/webapp/");
  const imagesCommon = path.resolve(webappCommon, "images");
  const themeConfig = path.resolve(webappCommon, "theme/theme.config");
  const themeSite = path.resolve(webappCommon, "theme");
  const suithemes = path.resolve(webappCommon, "suithemes");
  const publicDir = path.resolve(common, "src/main/public");
  return {
    root: "src/main/webapp",
    publicDir: publicDir,
    resolve: {
      alias: [
        {
          find: "@sjs",
          replacement: sjs,
        },
        {
          find: "/common",
          replacement: webappCommon,
        },
        {
          find: "/images",
          replacement: imagesCommon,
        },
        {
          find: "../../theme.config",
          replacement: themeConfig,
        },
        {
          find: "theme/site",
          replacement: themeSite,
        },
        {
          find: "suithemes",
          replacement: suithemes,
        },
      ],
    },
    server: {
      strictPort: true,
      port: 8080,
      https: {
        key: fs.readFileSync("server.key"),
        cert: fs.readFileSync("server.cert"),
      },
      watch: {
        ignored: [
          function ignoreThisPath(_path) {
            const sjsIgnored =
              _path.includes("/target/stream") ||
              _path.includes("/zinc/") ||
              _path.includes("/classes");
            return sjsIgnored;
          },
        ],
      },
      proxy: {
        "conf.json": {
          rewrite: (path) => {
            console.log(path);
            path.replace(/^\/conf.json$/, "/conf/development.conf.json");
          },
        },
      },
    },
    build: {
      terserOptions: {
        sourceMap: false,
      },
      outDir: path.resolve(__dirname, "../static"),
    },
    plugins: [reactRefresh(), fontImport],
  };
};
