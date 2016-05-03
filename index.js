var Elm = require( './src/Main' );

require('./gipher.css')

Elm.embed( Elm.Main, document.getElementById( 'main' ), { swap: true });
