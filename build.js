const path = require("path")
const fs = require("fs")
const process = require("process")
const size = require("human-format")
const { build } = require("vite")
const { minify } = require("terser")
const humanFormat = require("human-format")
const brotliSize = require("brotli-size")
const config = require("./vite.config.js")

const outDir = path.resolve(__dirname, "heroku/static")
const terserOptions =  {
  sourceMap: false,
  nameCache: {},
  format: {
    comments: false,
    ecma: 2015
  },
  mangle: {
    properties: {
      debug: false,
      reserved: ['$classData', 'main', 'toString', 'constructor', 'length', 'call', 'apply', 'NaN', 'Infinity', 'undefined'],
      // Basically, every root package except Lcrystal and Lexplore. For some reason it breaks dynamic import of the help system.
      regex: /^(\$m_|.*__f_|.*__F\d?_|.*__O_|.*Ljava|.*Lcats|.*Ljapgolly|.*Lfs2|.*Lorg|.*Lcom|.*Lio|.*Leu|.*Lclue|.*Llucuma|.*Lreact)/,
    }
  }
}

var i = 1
function runTerserOn(fileName, length) {
  process.stdout.write(`Minifying ${i++}/${length}: ${fileName}...`)
  const absolute = path.join(outDir, fileName)
  const original = fs.readFileSync(absolute, "utf8")
  minify(original, terserOptions).then( minified => {
    fs.writeFileSync(absolute, minified.code, "utf8")
    const fromSize = original.length
    const toSize = minified.code.length
    const ratio = (toSize / fromSize) * 100
    const fromBrotli = brotliSize.sync(original)
    const toBrotli = brotliSize.sync(minified.code)
    const brotliRatio = (toBrotli / fromBrotli) * 100
    process.stdout.write(` ${humanFormat.bytes(fromSize, { prefix: 'Ki' })} --> ${humanFormat.bytes(toSize, { prefix: 'Ki' })} (${ratio.toFixed(2)}%)` +
                          ` / brotli: ${humanFormat.bytes(fromBrotli, { prefix: 'Ki' })} --> ${humanFormat.bytes(toBrotli, { prefix: 'Ki' })} (${brotliRatio.toFixed(2)}%) \n`)
  })
}

;(async () => {
  const rollupOutput = await build(config)
  const jsChunks = rollupOutput.output.map(chunk => chunk.fileName).filter(fileName => fileName.endsWith(".js"))

  for (const fileName of jsChunks) {
    await runTerserOn(fileName, jsChunks.length)
  }
})()