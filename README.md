# temu - the terminal emulator we have at home

A terminal emulator recreating the aesthetics of old sci-fi movie computer displays.

## Prerequisites

`temu` links against several native (non-Haskell) libraries via `pkg-config`. Install these before building:

- `pkg-config` — used by the build to locate the native libraries below
- `sdl2` — windowing and input
- `sdl2_ttf` — font rendering
- `libvterm` — terminal state machine (linked directly)

### macOS (Homebrew)

```sh
brew install pkg-config sdl2 sdl2_ttf libvterm
```

### Debian / Ubuntu

```sh
sudo apt install pkg-config libsdl2-dev libsdl2-ttf-dev libvterm-dev
```

## Building

```sh
stack build
```
