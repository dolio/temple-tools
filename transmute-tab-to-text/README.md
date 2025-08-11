# Transume Tab to Text

## Info

This program mediates between ToEE table files and a more human consumable
textual format. The intention is that data can be edited using the latter
format, then compiled back to a table format understood by ToEE.

***WARNING***: this tool does not understand every line of of the
respective table files that actually ship with ToEE. When this happens, the
corresponding entries will be discarded in the output. So, if you run this
on an original ToEE file, edit the output, then compile back, you **will**
lose some of the original entries.

Instead, if possible, you should use this tool to produce more modest .tab
files that can be used to override the original files using e.g.
[Temple+](https://github.com/GrognardsFromHell/TemplePlus). Then you can
verify that your own work conforms to the strictures of the tool, which is
likely much more feasible. It's also likely that your own work actually
does conform to the tool's expectations, as many of the failing ToEE lines
are nonsense.

## Suported formats

At the moment, only one table format is supported.

### Help

Help tables consist of 6 columns

1. A unique tag identifying the help topic
2. The tag of the parent topic. This is jumped to if you press the 'up'
   button in the help interface.
3. A tag overriding the previous sibling of the topic.
4. The tags for the spell lists that the topic appears on
5. The title text of the topic.
6. The body text of the topic.

Columns 2-4 can be blank (so can 5 and 6, but that's unlikely to be
desired).

Column 3 is barely used in the help table. ToEE automatically calculates
help topic siblings based on the parents (the order of the siblings is the
order the topics occur in the help file). If Column 3 is specified, then
the 'previous topic' button in the interface will jump to the corresponding
topic, but **the 'next topic' button will be broken**.

So, this seems to be a legacy column that was abandoned. Given the above,
`transmute-tab-to-text` just doesn't support this column. It's thrown away
when translating to text, and will always be blank when it produces a .tab
file.

The textual format for a help entry looks like

    {{{
    topic-tag: ...
    parent-tag: ...
    spell-lists: ...
    title: ...
    |||
    ...
    }}}

The top 4 items correspond to columns 1,2,4 and 5. Order does not matter,
because the text before `:` identifies the column. Items can also be
omitted if they should just be blank. `title` text is trimmed of leading
and trailing spaces.

The portion of the entry strictly between the `|||` and `}}}` lines
corresponds to column 6. This requires some translation, though, because
obviously you can have multi-line help text, but it cannot be literally put
into the .tab file, since each entry is a _single_ line.

In the .tab file, line breaks are represented as the ASCII vertical tab
character (code 11; I'll write it `\v`). `transmute-tab-to-text` will
therefore parse the body text, and translate any line separators to the
.tab format, meaning that text like

    Meleny
      is

    nice

will become

    Meleny\v  is\v\vnice

in the table file. In game, the text will appear similar to the multi-line
text above.

`transmute-tab-to-text` does not do any trimming or joining of the lines in
the body. This is because many help entries use spaces and single line
breaks to make things that look like tables in the help. If you want to
write a paragraph that is automatically line-broken by the game, it should
be _all on one line_ in the body (_unlike_ the paragraphs in this markdown
file). If you manually break lines in a paragraph, it may look strange in
the in-game help, because of differences between fix and variable width
fonts (or just differences between character widths in different fonts).

Under no circumstances should any of the text contain tab characters,
because those have special meaning in the tab file, and as far as I know,
there is no alternate way to encode them. If you put tab characters in your
text, and the tool doesn't detect them and complain at you, then the result
will likely be an invalid .tab file.

## Usage

Every invocation of `transmute-tab-to-text` involves at least two arguments

    -i FILE  -- specifies the input file
    -o FILE  -- specifies the output file

Which sorts of file are expected for each depends on the mode you are
running in

1. The default mode turns a `.tab` file into a `.txt` file
2. Passing the `--reverse` flag turns a `.txt` file back into a `.tab` file
3. Passing the `--test-text` will consume and product `.txt` files, so that
   you can check that the result is (approximately) the same as the input,
   and no information is being lost.
4. Passing `--test-tab` is similar, but consumes and produces a `.tab`
   file.

A `-f FORMAT` option also exists, but right now only the `help` format is
understood, and it is the default.

## Building

`transmute-tab-to-text` is written in Haskell. To build it, you must obtain
a Haskell installation. Visit

    https://www.haskell.org/

to learn how to get set up. GHC 9.10 was used to develop this, and the
dependencies specified are likely to require something approximately that
new.

The project is set up to build with `cabal`, and once you have a Haskell
setup, should be able to be installed just with

    cabal install transmute-tab-to-text
