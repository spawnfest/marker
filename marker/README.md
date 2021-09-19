# marker

`marker` is an OTP library for parsing and rendering Markdown documents.

Currently it supports a most elements of the CommonMark specification
listed [here](https://commonmark.org/help/). See root `README.md` for details.

The goal is to support full [CommonMark specification](https://spec.commonmark.org/0.30/)
and then add add support for the other ones,
e.g. [Github Flavored Markdown](https://github.github.com/gfm/).


## Build

To build the library just run `make`. The default target build the library
and runs unit tests as you can read below:

```shell
$ make help
build                          Builds library source with rebar3
default                        Builds the sources and run unit tests.
test                           Runs Eunit tests for all modules.
```

## API

`marker` library consists of two modules and each of them exports one function:
* `marker` which handles parsing of Markdown documents and exports:
    * `markdown/1` function accepting a string with Markdown document
      and returning its tree representation
* `render` which handles HTML rendering and exports:
    * `html/1` function accepting the tree returned by `markdown/1`
      and returning the string with HTML code, ready to display.

### Tree structure

The tree returned by the `markdown/1` function has a regular recursive structure
with text elements as leaves. Each element may be in either this form for
non-leaves:

```
{<type_atom>, [<child_element>,...]}
```

or this - leaves:

```
{<type_atom>, <text>}
```

The `<type_atom>` above can be one of these:
* `document` - the root of the tree

Non-leaves, block elements:
* `horizontal_line` - rendered as `<hr>`
* `block_quote` - rendered as `<blockquote>`
* `bullet_list` - rendered as `<ul>`
* `list_item` - rendered as `<li>`
* `paragraph` - rendered as `<p>`
* `soft_break` - rendred as `<br>`

Leaves:
* `inline_code` - rendered as `<code>`
* `italic` - rendered as `<i>`
* `emph` - rendered as `<b>`
* `heading<1-6>` - rendered as `<h<1-6>>`
* `str` - rendered as regular text
