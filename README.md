# Temple Tools

## Info

This is a repository collecting some custom tools for working with data
files for the Temple of Elemental Evil PC game. I expect most of the tools
won't be very large, so don't really need separate repositories.

The repository is structured as a [Haskell](https://www.haskell.org/)
project using [Cabal](https://cabal.readthedocs.io/en/latest/index.html)
for the build tools. The whole repo is set up as one project that allows
building all the tools from the main directory if so desired (since that
will be how I work most of the time).

## Tools

- discern-dat

  This tool extracts Troika DAT archives, which are used to hold most of
  the data files. This is necessary if you want to see most of the
  individual files from the original game.

  The format isn't complicated, but the extractors I know of are Windows
  executables, so require emulation to run on other systems. This tool
  should work on any system that GHC and the Haskell `zlib` package
  support.
