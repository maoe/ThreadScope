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
threadscope
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
threadscope
```

### Windows

Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows) and [MSYS2](http://www.msys2.org).

On an MSYS2 MinGW shell:

```sh
pacman -S $MINGW_PACKAGE_PREFIX-{gtk2,pkg-config}
echo 'export PATH=$APPDATA/local/bin:$PATH' >> ~/.profile
source ~/.profile
stack install --skip-msys threadscope
threadscope
```
