# Gipher

http://gipher.co

Tinder-like application allowing you to swipe among trending gifs (using giphy API).

It's been a while I wanted to try out elm, I've always been attracted by FRP and programming with signals.

This attempt gave birth to gipher. Try it out [here](http://gipher.co).

Gipher has the main features we can find in any modern single page application:
- login with facebook
- multiple pages
- live data sync with firebase
- nice animations

It's a good example showing how you can structure an elm application using signals, everything wired up with start-app package.

This project is 99.9% elm, it contains only 30 lines of css, the rest of the styling is inline in elm (no javascript at all :heart_eyes:).

### Packages used (elm)

###### `Apanatshka/elm-signal-extra` provides more advanced features to manipulate signals

###### `Dandandan/Easing` is an amazing library to work with easings (useful for all kind of animations)

###### `ThomasWeiser/elmfire` enables you to interact with firebase in the most 'elmish' way

###### And of course a big thank you to evancz for all his work around elm:
  `evancz/elm-effects`
  `evancz/elm-html`
  `evancz/elm-http`
  `evancz/start-app`

## TODO

- add gif search by keyword
- reload more gifs
- make it work with touch events [(elm-html issue raised)](https://github.com/evancz/elm-html/issues/99 )
