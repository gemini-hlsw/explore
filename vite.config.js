const { defineConfig } = require('vite');
const react = require('@vitejs/plugin-react');
const { visualizer } = require('rollup-plugin-visualizer');
const path = require('path');
const fs = require('fs');
const ViteFonts = require('vite-plugin-fonts');
const mkcert = require('vite-plugin-mkcert');
const { VitePWA } = require('vite-plugin-pwa');

const fontImport = ViteFonts.Plugin({
  google: {
    families: [
      {
        name: 'Lato',
        styles: 'ital,wght@0,400;0,700;1,400;1,700',
      },
    ],
  },
});

// Configuration to cache aladin images
const imageCache = ({ name, pattern }) => ({
  urlPattern: pattern,
  handler: 'CacheFirst',
  options: {
    cacheName: name,
    expiration: {
      maxEntries: 2500,
      maxAgeSeconds: 60 * 60 * 24 * 14, // 1week
    },
    cacheableResponse: {
      statuses: [200],
    },
  },
});

// Configuration for itc
const itcCache = ({ name, pattern }) => ({
  urlPattern: pattern,
  handler: 'CacheFirst',
  options: {
    cacheName: name,
    expiration: {
      maxEntries: 5000,
      maxAgeSeconds: 60 * 60 * 24 * 7, // 1week
    },
    cacheableResponse: {
      statuses: [200],
    },
  },
});

// https://vitejs.dev/config/
module.exports = ({ command, mode }) => {
  const scalaClassesDir = path.resolve(__dirname, 'explore/target/scala-3.1.3');
  const isProduction = mode == 'production';
  const sjs = isProduction
    ? path.resolve(scalaClassesDir, 'explore-opt')
    : path.resolve(scalaClassesDir, 'explore-fastopt');
  const workersScalaClassesDir = path.resolve(
    __dirname,
    'workers/target/scala-3.1.3'
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
  fs.mkdir(publicDirDev, (err) => {
    fs.copyFileSync(
      path.resolve(publicDirProd, 'development.conf.json'),
      path.resolve(publicDirDev, 'conf.json')
    );
    fs.copyFileSync(
      path.resolve(publicDirProd, 'instrument_spectroscopy_matrix.csv'),
      path.resolve(publicDirDev, 'instrument_spectroscopy_matrix.csv')
    );
  });

  const publicDir = mode == 'production' ? publicDirProd : publicDirDev;
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
      ],
    },
    css: {
      preprocessorOptions: {
        scss: {
          charset: false,
        },
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
      proxy: {
        'conf.json': {
          rewrite: (path) => {
            path.replace(/^\/conf.json$/, '/conf/development.conf.json');
          },
        },
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
          ecma: 2015,
        },
      },
      rollupOptions: {
        plugins: rollupPlugins,
      },
      outDir: path.resolve(__dirname, 'heroku/static'),
    },
    plugins: [
      isProduction
        ? null
        : mkcert.default({ hosts: ['localhost', 'local.lucuma.xyz'] }),
      react(),
      fontImport,
      VitePWA({
        workbox: {
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
              pattern: /^https:\/\/(itc-staging.lucuma.xyz|itc.gpp.gemini.edu)\/itc/,
              name: 'itc-cache',
            }),
            itcCache({
              pattern: /^https:\/\/cors-proxy.(lucuma.xyz|gpp.gemini.edu)\/http:\/\/aladin.unistra.fr\/java\/nph-aladin.*/,
              name: 'cors-cache',
            })
          ],
        },
      }),
    ],
  };
};
