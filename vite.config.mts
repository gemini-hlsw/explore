import type { PathLike } from 'fs';
import fs from 'fs/promises';
import { dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import path from 'path';
import type { PluginCreator } from 'postcss';
import Unfonts from 'unplugin-fonts/vite';
import { defineConfig, UserConfig } from 'vite';
import mkcert from 'vite-plugin-mkcert';
import { VitePWA } from 'vite-plugin-pwa';
import type { RuntimeCaching } from 'workbox-build';
import env from 'vite-plugin-env-compatible';

const scalaVersion = '3.5.2';

const fixCssRoot: PluginCreator<void> = () => {
  return {
    postcssPlugin: 'postcss-fix-nested-root',
    Once(root) {
      root.walkRules((rule) => {
        if (rule.selector.includes(' :root')) {
          rule.selector = rule.selector.replace(' :root', '');
        }
      });
    },
  };
};
fixCssRoot.postcss = true;

const fontImport = Unfonts({
  fontsource: {
    families: ['Lato'],
  },
});

/**
 * Configuration to cache aladin images
 */
const imageCache = ({
  name,
  pattern,
}: {
  name: string;
  pattern: RuntimeCaching['urlPattern'];
}): RuntimeCaching => ({
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
 */
const itcCache = ({
  name,
  pattern,
}: {
  name: string;
  pattern: RuntimeCaching['urlPattern'];
}): RuntimeCaching => ({
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
 */
const pathExists = async (path: PathLike) => {
  try {
    await fs.access(path, fs.constants.F_OK);
    return true;
  } catch (err) {
    return false;
  }
};

// https://vitejs.dev/config/
export default defineConfig(async ({ mode }) => {
  const _dirname =
    typeof __dirname !== 'undefined' ? __dirname : dirname(fileURLToPath(import.meta.url));
  const scalaClassesDir = path.resolve(_dirname, `explore/target/scala-${scalaVersion}`);
  const isProduction = mode === 'production';
  const sjs = isProduction
    ? path.resolve(scalaClassesDir, `explore-opt`)
    : path.resolve(scalaClassesDir, `explore-fastopt`);
  const workersScalaClassesDir = path.resolve(_dirname, `workers/target/scala-${scalaVersion}`);
  const workersSjs = isProduction
    ? path.resolve(workersScalaClassesDir, 'workers-opt')
    : path.resolve(workersScalaClassesDir, 'workers-fastopt');
  const common = path.resolve(_dirname, 'common/');
  const webappCommon = path.resolve(common, 'src/main/webapp/');
  const imagesCommon = path.resolve(webappCommon, 'images');
  const publicDirProd = path.resolve(common, 'src/main/public');
  const publicDirDev = path.resolve(common, 'src/main/publicdev');
  const lucumaCss = path.resolve(_dirname, `explore/target/lucuma-css`);

  if (!(await pathExists(publicDirDev))) {
    await fs.mkdir(publicDirDev);
  }
  const localConf = path.resolve(publicDirProd, 'local.conf.json');
  const devConf = path.resolve(publicDirProd, 'environments.conf.json');

  const publicDirProdFiles = (await fs.readdir(publicDirProd)).filter(
    (file) =>
      !file.endsWith('local.conf.json') &&
      !file.endsWith('environments.conf.json') &&
      !file.endsWith('README.txt'),
  );

  await Promise.all([
    fs.copyFile(
      (await pathExists(localConf)) ? localConf : devConf,
      path.resolve(publicDirDev, 'environments.conf.json'),
    ),
    ...publicDirProdFiles.map((file) =>
      fs.copyFile(path.resolve(publicDirProd, file), path.resolve(publicDirDev, file)),
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
      fs: {
        strict: true,
      },
      host: '0.0.0.0',
      port: 8080,
      cors: { origin: '*' },
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
      // https://vitejs.dev/guide/performance.html#warm-up-frequently-used-files
      // @ts-expect-error doesn't exist in type definition, but it's in the docs
      warmup: {
        clientFiles: [
          path.resolve(sjs, '*.js'),
          path.resolve(webappCommon, 'sass/*.scss'),
          path.resolve(lucumaCss, '*.scss'),
        ],
      },
    },
    build: {
      emptyOutDir: true,
      chunkSizeWarningLimit: 20000,
      outDir: path.resolve(_dirname, 'heroku/static'),
    },
    worker: {
      format: 'es', // We need this for workers to be able to do dynamic imports.
    },
    plugins: [
      env(),
      mkcert({ hosts: ['localhost', 'local.lucuma.xyz'] }),
      fontImport,
      VitePWA({
        injectRegister: 'inline',
        workbox: {
          globPatterns: ['**/*.{js,css,html,wasm}'],
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
  } satisfies UserConfig;
});
