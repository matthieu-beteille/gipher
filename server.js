'use strict'

const path = require('path')
const express = require('express')
const compression = require('compression')

const app = express()

// Enable gzip
app.use(compression())

// Serve dist
app.use(express.static(path.resolve(__dirname, 'dist')))

// View render
app.set('views', path.join(__dirname, 'dist'))
app.set('view engine', 'ejs')

// Render files
app.get('*', function (req, res) {
  res.render('index')
})

const server = app.listen((process.env.PORT || 9000), function () {
  console.log('Express server listening at http://%s:%s', server.address().address, server.address().port)
})
