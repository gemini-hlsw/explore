// @ts-check
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import { visualizer } from 'rollup-plugin-visualizer';
import path from 'path';
import fs from 'fs/promises';
import mkcert from 'vite-plugin-mkcert';
import { VitePluginFonts } from 'vite-plugin-fonts';
import { VitePWA } from 'vite-plugin-pwa';
// import wasm from "vite-plugin-wasm";
// import topLevelAwait from "vite-plugin-top-level-await";

const fixCssRoot = (opts = {}) => {
  return {
    postcssPlugin: 'postcss-fix-nested-root',
    Once(root, { result }) {
      root.walkRules((rule) => {
        if (rule.selector.includes(' :root')) {
          rule.selector = rule.selector.replace(' :root', '');
        }
      });
    },
  };
};
/**
 * Refine type to 'true' instead of 'boolean'
 * @type {true}
 */
fixCssRoot.postcss = true;

const fontImport = VitePluginFonts({
  google: {
    families: [
      {
        name: 'Lato',
        styles: 'ital,wght@0,400;0,700;1,400;1,700',
      },
    ],
  },
});

/**
 * Configuration to cache aladin images
 * @param {{name: string, pattern: RegExp}} param0
 * @returns {import('workbox-build').RuntimeCaching}
 */
const imageCache = ({ name, pattern }) => ({
  urlPattern: pattern,
  handler: 'CacheFirst',
  options: {
    cacheName: name,
    expiration: {
      purgeOnQuotaError: true,
      maxEntries: 2500,
      maxAgeSeconds: 60 * 60 * 24 * 14, // 1week
    },
    cacheableResponse: {
      statuses: [200],
    },
  },
});

/**
 * Configuration for itc
 * @param {{name: string, pattern: RegExp}} param0
 * @returns {import('workbox-build').RuntimeCaching}
 */
const itcCache = ({ name, pattern }) => ({
  urlPattern: pattern,
  handler: 'CacheFirst',
  options: {
    cacheName: name,
    expiration: {
      purgeOnQuotaError: true,
      maxEntries: 5000,
      maxAgeSeconds: 60 * 60 * 24, // 1day
    },
    cacheableResponse: {
      statuses: [200],
    },
  },
});

/**
 * Check if a file or directory exists
 * @param {import('fs').PathLike} path
 * @returns
 */
const pathExists = async (path) => {
  try {
    await fs.access(path, fs.constants.F_OK);
    return true;
  } catch (err) {
    return false;
  }
};

// https://vitejs.dev/config/
export default defineConfig(async ({ mode }) => {
  const scalaClassesDir = path.resolve(__dirname, `explore/target/scala-3.3.0`);
  const isProduction = mode === 'production';
  const sjs = isProduction
    ? path.resolve(scalaClassesDir, `explore-opt`)
    : path.resolve(scalaClassesDir, `explore-fastopt`);
  const workersScalaClassesDir = path.resolve(
    __dirname,
    'workers/target/scala-3.3.0'
  );
  const workersSjs = isProduction
    ? path.resolve(workersScalaClassesDir, 'workers-opt')
    : path.resolve(workersScalaClassesDir, 'workers-fastopt');
  const rollupPlugins = isProduction ? [] : [visualizer()];
  const common = path.resolve(__dirname, 'common/');
  const webappCommon = path.resolve(common, 'src/main/webapp/');
  const imagesCommon = path.resolve(webappCommon, 'images');
  const themeConfig = path.resolve(webappCommon, 'theme/theme.config');
  const themeSite = path.resolve(webappCommon, 'theme');
  const suithemes = path.resolve(webappCommon, 'suithemes');
  const publicDirProd = path.resolve(common, 'src/main/public');
  const publicDirDev = path.resolve(common, 'src/main/publicdev');
  const lucumaCss = path.resolve(__dirname, `explore/target/lucuma-css`);

  if (!(await pathExists(publicDirDev))) {
    await fs.mkdir(publicDirDev);
  }
  const localConf = path.resolve(publicDirProd, 'local.conf.json');
  const devConf = path.resolve(publicDirProd, 'environments.conf.json');

  const publicDirProdFiles = (await fs.readdir(publicDirProd)).filter(
    (file) =>
      !file.endsWith('local.conf.json') &&
      !file.endsWith('environments.conf.json') &&
      !file.endsWith('README.txt')
  );

  await Promise.all([
    fs.copyFile(
      (await pathExists(localConf)) ? localConf : devConf,
      path.resolve(publicDirDev, 'environments.conf.json')
    ),
    ...publicDirProdFiles.map((file) =>
      fs.copyFile(
        path.resolve(publicDirProd, file),
        path.resolve(publicDirDev, file)
      )
    ),
  ]);

  const publicDir = mode === 'production' ? publicDirProd : publicDirDev;

  return {
    // TODO Remove this if we get EnvironmentPlugin to work.
    root: 'explore/src/main/webapp',
    publicDir: publicDir,
    envPrefix: ['VITE_', 'CATS_EFFECT_'],
    resolve: {
      dedupe: ['react-is'],
      alias: [
        {
          find: 'process',
          replacement: 'process/browser',
        },
        {
          find: '@sjs',
          replacement: sjs,
        },
        {
          find: '@workers',
          replacement: workersSjs,
        },
        {
          find: '/common',
          replacement: webappCommon,
        },
        {
          find: '/images',
          replacement: imagesCommon,
        },
        {
          find: '../../theme.config',
          replacement: themeConfig,
        },
        {
          find: 'theme/site',
          replacement: themeSite,
        },
        {
          find: 'suithemes',
          replacement: suithemes,
        },
        {
          find: '/lucuma-css',
          replacement: lucumaCss,
        },
      ],
    },
    css: {
      preprocessorOptions: {
        scss: {
          charset: false,
        },
      },
      postcss: {
        plugins: [fixCssRoot],
      },
    },
    server: {
      strictPort: true,
      fsServe: {
        strict: true,
      },
      host: '0.0.0.0',
      port: 8080,
      https: true,
      watch: {
        ignored: [
          function ignoreThisPath(_path) {
            const sjsIgnored =
              _path.includes('/target/stream') ||
              _path.includes('/zinc/') ||
              _path.includes('/classes') ||
              _path.endsWith('.tmp');
            return sjsIgnored;
          },
        ],
      },
    },
    build: {
      emptyOutDir: true,
      chunkSizeWarningLimit: 20000,
      terserOptions: {
        sourceMap: false,
        compress: {
          passes: 2,
          toplevel: true,
          ecma: 2020,
        },
      },
      rollupOptions: {
        plugins: rollupPlugins,
      },
      minify: 'terser',
      outDir: path.resolve(__dirname, 'heroku/static'),
    },
    worker: {
      format: 'es', // We need this for workers to be able to do dynamic imports.
      // We need these to allow wasm modules on the workres
      // plugins: [
      //   wasm(),
      //   topLevelAwait()
      // ]
    },
    plugins: [
      // wasm(),
      // topLevelAwait(),
      mkcert({ hosts: ['localhost', 'local.lucuma.xyz'] }),
      react(),
      fontImport,
      VitePWA({
        injectRegister: 'inline',
        workbox: {
          globPatterns: ['**/*.{js,html,wasm}'],
          maximumFileSizeToCacheInBytes: 30000000, // sjs produce large ffiles
          // Cache aladin images
          runtimeCaching: [
            imageCache({
              pattern: /^https:\/\/simbad.u-strasbg.fr\/simbad\/sim-id/,
              name: 'simbad',
            }),
            imageCache({
              pattern: /^https:\/\/alasky.u-strasbg.fr\/DSS/,
              name: 'aladin-images',
            }),
            itcCache({
              pattern:
                /^https:\/\/cors-proxy.(lucuma.xyz|gpp.gemini.edu)\/http:\/\/aladin.unistra.fr\/java\/nph-aladin.*/,
              name: 'cors-cache',
            }),
          ],
        },
      }),
    ],
  };
});
