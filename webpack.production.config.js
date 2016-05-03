const webpack = require('webpack')

var config = Object.assign({}, require('./webpack.base.config.js'))

config.plugins = Array.prototype.concat(
  config.plugins, [
    new webpack.optimize.DedupePlugin(),
    new webpack.optimize.UglifyJsPlugin({compress: {warnings: false}}),
  ]
)

module.exports = config
