const webpack = require('webpack')
var config = Object.assign({}, require('./webpack.base.config.js'))

config.entry.unshift(
  'webpack-dev-server/client?http://0.0.0.0:8080',
  'webpack/hot/only-dev-server'
)

config.module.loaders
  .filter(item => item.id === 'elm')
  .map(item => item.loaders.unshift('elm-hot'))

config.plugins.push(
  new webpack.HotModuleReplacementPlugin()
)

module.exports = config
