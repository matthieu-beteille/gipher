const path = require('path')

const regexp = {
  elm: /\.elm?$/,
  style: /\.(styl|css)$/
}

const config = {
  entry: ['./index.js'],
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'bundle.js'
  },
  devtool: 'source-map',
  module: {
    noParse: regexp.elm,
    loaders: [
      {
        id: 'elm',
        test: regexp.elm,
        loaders: ['elm-webpack'],
        exclude: [/elm-stuff/, /node_modules/]
      },
      {
        id: 'style',
        test: regexp.style,
        loader: 'style-loader!css-loader',
        exclude: /(node_modules)/
      }
    ]
  },
  plugins: [],
  resolve: {
    modulesDirectories: ['node_modules/'],
    extensions: ['', '.js', '.elm']
  }
}

module.exports = config
