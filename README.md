# Gipher

Tinder-like application allowing you to swipe among trending gifs (using giphy API).

It's been a while I wanted to give elm an attempt, here is my first one. Gipher has the main features we can find in any modern single page application:
- login with facebook
- multiple pages
- live data sync with firebase
- animations

It's a good example showing how you can structure an elm application using signals, everything wired up with start-app package.

To achieve that, I only used elm libraries (no javascript at all :heart_eyes:), here is the list.

### elm-libraries used

###### If you need more advanced features to manipulate signals, use the great
   `Apanatshka/elm-signal-extra`

###### Amazing library to work with easings (useful for all kind of animations)
   `Dandandan/Easing`

###### Awesome library enabling you to interact with firebase in the most 'elmish' way
  `ThomasWeiser/elmfire`

###### And of course a big thank you to evancz for all his work around elm:
  `evancz/elm-effects
  evancz/elm-html
  evancz/elm-http
  evancz/start-app`

## TODO

- add gif search by keyword
- reload more gifs
- make it work with touch events [(elm-html issue raised)](https://github.com/evancz/elm-html/issues/99 )
