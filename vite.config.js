import reactRefresh from "@vitejs/plugin-react-refresh";
import { visualizer } from 'rollup-plugin-visualizer';
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
  const scalaClassesDir = path.resolve(__dirname, "explore/target/scala-2.13");
  const isProduction = mode == "production";
  const sjs =
    isProduction
      ? path.resolve(scalaClassesDir, "explore-opt")
      : path.resolve(scalaClassesDir, "explore-fastopt");
  const rollupPlugins = isProduction ? [] : [visualizer()];
  const common = path.resolve(__dirname, "common/");
  const webappCommon = path.resolve(common, "src/main/webapp/");
  const imagesCommon = path.resolve(webappCommon, "images");
  const themeConfig = path.resolve(webappCommon, "theme/theme.config");
  const themeSite = path.resolve(webappCommon, "theme");
  const suithemes = path.resolve(webappCommon, "suithemes");
  const publicDirProd = path.resolve(common, "src/main/public");
  const publicDirDev = path.resolve(common, "src/main/publicdev");
  fs.mkdir(publicDirDev, (err) => {
    fs.copyFileSync(
      path.resolve(publicDirProd, "development.conf.json"),
      path.resolve(publicDirDev, "conf.json")
    );
    fs.copyFileSync(
      path.resolve(publicDirProd, "instrument_spectroscopy_matrix.csv"),
      path.resolve(publicDirDev, "instrument_spectroscopy_matrix.csv")
    );
  });

  const publicDir = mode == "production" ? publicDirProd : publicDirDev;
  return {
    root: "explore/src/main/webapp",
    publicDir: publicDir,
    resolve: {
      dedupe: ["react-is"],
      alias: [
        {
          find: "process",
          replacement: "process/browser"
        },
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
      fsServe: {
        strict: true
      },
      host: "0.0.0.0",
      port: 8080,
      https:
        mode == "production"
          ? {}
          : {
              key: fs.readFileSync("server.key"),
              cert: fs.readFileSync("server.cert"),
            },
      watch: {
        ignored: [
          function ignoreThisPath(_path) {
            const sjsIgnored =
              _path.includes("/target/stream") ||
              _path.includes("/zinc/") ||
              _path.includes("/classes") ||
              _path.endsWith(".tmp");
            return sjsIgnored;
          },
        ],
      },
      proxy: {
        "conf.json": {
          rewrite: (path) => {
            path.replace(/^\/conf.json$/, "/conf/development.conf.json");
          },
        },
      },
    },
    build: {
      minify: 'terser',
      rollupOptions: {
        plugins: rollupPlugins
      },
      terserOptions: {
        sourceMap: false,
        nameCache: {},
        format: {
          comments: false,
        },
        mangle: {
          properties: {
            debug: false,
            keep_quoted: true,
            reserved: ['$classData', 'main', 'toString', 'constructor', 'length', 'call', 'apply', 'NaN', 'Infinity', 'undefined'],
            regex: /^(\$m_|loadHelp.*|.*__f_|Ljava|cats\$)/,
          }
        },
      },
      outDir: path.resolve(__dirname, "heroku/static"),
    },
    plugins: [reactRefresh(), fontImport],
  };
};
