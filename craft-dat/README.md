# Craft (Dat)

## Info

This is an implementation of a quick-and-dirty extractor/creator for Troika
DAT files. It has three modes, `list`, `disjoin` and `fabricate`.

The `list` mode simply lists the files stored in a DAT archive, showing the
directories/files that _would_ be created in disjoin mode. Whether or not a
stored file is compressed is also indicated. Usage is:

  > craft-dat list FILE

The `disjoin` mode actually extracts the entire contents of the archive.
Usage is:

  > craft-dat disjoin FILE

An optional `-d` argument specifies a directory to extract the files to.
This works both in `list` and `disjoin` mode, with the former showing the
locations of the files that will be created including the directory.

The `fabricate` mode is for creating archives. It takes a directory as an
argument, and creates a DAT file containing the entire contents of that
directory. An optional `-o` argument specifies the name of the archive to be
created. By default, the directory name is used, but with `.dat` appended.

A `-v` switch will put the program into 'verbose' mode, which will print
out some extra information while running in some modes.

Some warnings:

- The program makes no effort to avoid overwriting files. Be careful
  extracting an archive if e.g. you've already extracted and edited some
  contents.
- Some malformed archives could cause the program to go into an infinite
  loop reassembling the directory structure. If that happens, then verbose
  mode will show that the program starts `Building directory tree` but
  never makes it to `Extracting files`.

Also included is an interval parsing grammar for the footer and file table
section of a DAT file. This is included mainly as documentation, as at the
time of this writing, only generation of JavaScript and Rust parsers are
supported, so the Haskell parser here is hand written. Also, we don't
really want to read the DAT contents fully into memory, but rather bounce
around in the DAT file extracting individual portions, so a bit of a custom
approach seems necessary.

## Building

The main program is written in Haskell. To build it, you must obtain a
Haskell installation. Visit

    https://www.haskell.org/

to learn how to get set up. GHC 9.10 was used to develop this, and the
dependencies specified are likely to require something approximately that
new.

The project is set up to build with `cabal`, and once you have a Haskell
setup, should be able to be installed just with

    cabal install craft-dat
