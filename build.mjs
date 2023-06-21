import path from 'path';
import { fileURLToPath } from 'url';
import fs from 'fs/promises';
import process from 'process';
import { build } from 'vite';
import { minify } from 'terser';
import humanFormat from 'human-format';
import brotliSize from 'brotli-size';
import config from './vite.config.mjs';

const __filename = fileURLToPath(import.meta.url);

const __dirname = path.dirname(__filename);

const outDir = path.resolve(__dirname, 'heroku/static');
const terserOptions = {
  sourceMap: false,
  nameCache: {},
  format: {
    comments: false,
    ecma: 2020,
  },
  mangle: {
    properties: {
      debug: false,
      reserved: [
        '$classData',
        'main',
        'toString',
        'constructor',
        'length',
        'call',
        'apply',
        'NaN',
        'Infinity',
        'undefined',
      ],
      // Basically, every root package except Lcrystal and Lexplore. For some reason it breaks dynamic import of the help system.
      regex:
        /^(\$m_|.*__f_|.*__F\d?_|.*__O_|.*Ljava|.*Lcats|.*Ljapgolly|.*Lfs2|.*Lorg|.*Lcom|.*Lio|.*Leu|.*Lclue|.*Llucuma|.*Lreact)/,
    },
  },
};

let i = 1;
async function runTerserOn(fileName, length) {
  process.stdout.write(`Minifying ${i++}/${length}: ${fileName}...`);
  const absolute = path.join(outDir, fileName);
  const original = await fs.readFile(absolute, 'utf8');
  const minified = await minify(original, terserOptions);
  await fs.writeFile(absolute, minified.code, 'utf8');
  const fromSize = original.length;
  const toSize = minified.code.length;
  const ratio = (toSize / fromSize) * 100;
  const [fromBrotli, toBrotli] = await Promise.all([
    brotliSize.default(original),
    brotliSize.default(minified.code),
  ]);
  const brotliRatio = (toBrotli / fromBrotli) * 100;
  process.stdout.write(
    ` ${humanFormat.bytes(fromSize, {
      prefix: 'Ki',
    })} --> ${humanFormat.bytes(toSize, { prefix: 'Ki' })} (${ratio.toFixed(
      2
    )}%)` +
      ` / brotli: ${humanFormat.bytes(fromBrotli, {
        prefix: 'Ki',
      })} --> ${humanFormat.bytes(toBrotli, {
        prefix: 'Ki',
      })} (${brotliRatio.toFixed(2)}%) \n`
  );
}

(async () => {
  const rollupOutput = await build(config);
  const jsChunks = rollupOutput.output
    .map((chunk) => chunk.fileName)
    .filter((fileName) => fileName.endsWith('.js'));

  for (const fileName of jsChunks) {
    await runTerserOn(fileName, jsChunks.length);
  }
})();
