# Gipher

http://gipher.co

[Product Hunt Page.](https://www.producthunt.com/tech/gipher)

![screenshot](https://ph-files.imgix.net/4c382360-6b03-46cd-959b-4682af46336b?auto=format&fit=max&h=570&w=430 "")

A Tinder-like application allowing you to swipe among trending gifs (using giphy API).

For a while I've wanted to try out elm. I've always been attracted to FRP (Functional Reactive Programming), and programming with signals.

This attempt gave birth to [Gipher](http://gipher.co).

Gipher has the main features we can find in any modern single page application:
- facebook authentication
- multiple sections
- live data sync with firebase
- animations

It's a good showcase of how an elm application is structured using signals, wired up with the start-app package.

This project is 99.9% elm, it contains only 30 lines of css, the rest of the styling is inline within elm (no javascript at all :heart_eyes:).

### Packages used (elm)

###### `Apanatshka/elm-signal-extra` provides more advanced features to manipulate signals

###### `Dandandan/Easing` is an amazing library to work with easings (useful for all kind of animations)

###### `ThomasWeiser/elmfire` enables you to interact with firebase in the most 'elmish' way

###### And of course a big thank you to [evancz](https://github.com/evancz) for all his work around elm:
  `evancz/elm-effects`
  `evancz/elm-html`
  `evancz/elm-http`
  `evancz/start-app`

## TODO

- add gif search by keyword
- reload more gifs
- make it work with touch events [(elm-html issue raised)](https://github.com/evancz/elm-html/issues/99 )
