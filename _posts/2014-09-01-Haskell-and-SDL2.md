title: Getting started with SDL2 in Haskell on Mac OS X
authors: Gil
tags: haskell, sdl2, graphics, game development, OS X

## Installation:
First let's start by installing the sdl2 library, we are going to use [brew][brew] so if you don't have it yet, now is a good time to install it.
Open up a terminal and type the following commands to install sdl2, sdl2\_image, and sdl2\_mixer:

```bash
brew install sdl2
brew install sdl2_image
brew install sdl2_mixer
```

Now let's install sdl2 binding for haskell from github:

```bash
git clone https://github.com/polarina/sdl2.git hlibs/sdl2
git clone https://github.com/ods94065/sdl2-image.git hlibs/sdl2-image

mkdir sdl2haskell && cd sdl2haskell
cabal sandbox init
cabal install ../hlibs/sdl2 ../hlibs/sdl2-image
```


[brew]: http://brew.sh
