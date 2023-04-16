import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import { visualizer } from 'rollup-plugin-visualizer';
import path from 'path';
import fs from 'fs';
import mkcert from 'vite-plugin-mkcert';
import { VitePluginFonts } from 'vite-plugin-fonts';
import { VitePWA } from 'vite-plugin-pwa';

const fixCssRoot = (opts = {}) => {
  return {
    postcssPlugin: 'postcss-fix-nested-root',
    Once(root, { result }) {
      root.walkRules(rule => {
        if (rule.selector.includes(' :root')) {
          rule.selector = rule.selector.replace(' :root', '');
        }
      });
    }
  }
}
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
export default defineConfig(({ command, mode }) => {
  const scalaClassesDir = path.resolve(
    __dirname,
    'explore/target/scala-3.2.2'
  );
  const isProduction = mode == 'production';
  const sjs = isProduction
    ? path.resolve(scalaClassesDir, 'explore-opt')
    : path.resolve(scalaClassesDir, 'explore-fastopt');
  const workersScalaClassesDir = path.resolve(
    __dirname,
    'workers/target/scala-3.2.2'
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
  const lucumaCss = path.resolve(__dirname, 'explore/target/lucuma-css')
  fs.mkdir(publicDirDev, (err) => {
    const localConf = path.resolve(publicDirProd, 'local.conf.json');
    const devConf = path.resolve(publicDirProd, 'development.conf.json');

    fs.copyFileSync(
      fs.existsSync(localConf) ? localConf : devConf,
      path.resolve(publicDirDev, 'conf.json')
    );
    fs.copyFileSync(
      path.resolve(publicDirProd, 'instrument_spectroscopy_matrix.csv'),
      path.resolve(publicDirDev, 'instrument_spectroscopy_matrix.csv')
    );
    fs.copyFileSync(
      path.resolve(publicDirProd, 'bracket.svg'),
      path.resolve(publicDirDev, 'bracket.svg')
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
        plugins: [fixCssRoot]
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
      minify: "terser",
      outDir: path.resolve(__dirname, 'heroku/static'),
    },
    worker: {
      format: 'es', // We need this for workers to be able to do dynamic imports.
    },
    plugins: [
      mkcert({ hosts: ['localhost', 'local.lucuma.xyz'] }),
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
              pattern:
                /^https:\/\/(itc-staging.lucuma.xyz|itc.gpp.gemini.edu)\/itc/,
              name: 'itc-cache',
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
