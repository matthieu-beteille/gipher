const webpack = require('webpack')
const path = require('path')
const fs = require('fs')

var config = Object.assign({}, require('./webpack.base.config.js'))

// adding an hash to the bundle filename
config.output.filename = '[hash].bundle.js'

config.plugins = Array.prototype.concat(
  config.plugins, [
    new webpack.optimize.DedupePlugin(),
    new webpack.optimize.UglifyJsPlugin({compress: {warnings: false}}),
    function onBuildComplete () {
      this.plugin('done', function (data) {
        var stats = data.toJson()

        replaceScriptTag(stats.assetsByChunkName.main[0])
      })
    }
  ]
)

function replaceScriptTag (newSrc) {
  var originalSrc = /bundle\.js/
  var indexPath = path.resolve(__dirname, 'index.html')
  var destinationPath = path.resolve(__dirname, 'dist', 'index.html')

  fs.readFile(indexPath, 'utf8', function (readErr, content) {
    if (readErr) {
      return console.log('reading error while trying to replace the bundle script tag', readErr)
    }
    content = content.replace(originalSrc, newSrc)

    fs.writeFile(destinationPath, content, 'utf8', function (writeErr) {
      if (writeErr) {
        return console.log('writing error while trying to replace the bundle script tag', readErr)
      }
    })
  })
}

module.exports = config
