# ThreadScope
[![Hackage](https://img.shields.io/hackage/v/threadscope.svg)](https://hackage.haskell.org/package/threadscope)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/threadscope.svg)](http://packdeps.haskellers.com/feed?needle=threadscope)
[![Build Status](https://travis-ci.org/haskell/ThreadScope.svg?branch=master)](https://travis-ci.org/haskell/ThreadScope)
[![Build status](https://ci.appveyor.com/api/projects/status/y6kd8pyh2f3qok4f?svg=true)](https://ci.appveyor.com/project/Mikolaj/threadscope)

## Installation

### Linux

GTK+2 is required to be installed. On Ubuntu-like systems:

```sh
sudo apt install libgtk2.0-dev
```

Then you can build threadscope using cabal:
```sh
cabal new-build
```

Or using stack:
```sh
stack setup
stack install
```

### macOS

GTK+ and gtk-mac-integration are required.

```sh
brew install gtk+ gtk-mac-integration
```

Then you can build threadscope using cabal:
```sh
cabal new-build --constraint="gtk +have-quartz-gtk"
```

Or using stack:
```sh
stack setup
stack install --flag gtk:have-quartz-gtk
```

### Windows

stack is the recommended tool to build threadscope on Windows.

```sh
stack setup
stack exec -- pacman --noconfirm --needed -Sy bash pacman pacman-mirrors msys2-runtime msys2-run
stack exec -- pacman --noconfirm -Syu
stack exec -- pacman --noconfirm -Syuu
stack exec -- pacman --noconfirm -S base-devel mingw-w64-x86_64-pkg-config mingw-w64-x86_64-toolchain mingw-w64-x86_64-gtk2
```
