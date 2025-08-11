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

- transmute-tab-to-text

  This tool translates between ToEE .tab files and a more human readable
  textual representation. This allows editing the latter representation
  with relative ease in a normal text editor and then transforming back
  into a format understood by the game.

  At the moment, only help tables are supported.

  Note: the transformation process may not perfectly preserve the original
  files. In the case of the textual representation, some parts are order
  insensitive, and the tool will reorder those parts into a canonical
  ordering. In the case of .tab files, actual ToEE files contain some lines
  that aren't understood by the tool, and those will be discarded. Most of
  these are nonsense lines, but it's possible some legitimate ones have
  been missed.

  So, it is advisable to _not_ run this tool on the original ToEE files
  with the expectation of recreating the original with some edits. A better
  methodology would be to use this tool to build your own override .tab
  files. Then you just need to ensure your own entries are understood by
  the tool, rather than any oddball ToEE ones.
