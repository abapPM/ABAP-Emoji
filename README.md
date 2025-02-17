![Version](https://img.shields.io/endpoint?url=https://shield.abappm.com/github/abapPM/ABAP-Emoji/src/zcl_emoji.clas.abap/c_version&label=Version&color=blue)

[![License](https://img.shields.io/github/license/abapPM/ABAP-Emoji?label=License&color=success)](LICENSE)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg?color=success)](https://github.com/abapPM/.github/blob/main/CODE_OF_CONDUCT.md)
[![REUSE Status](https://api.reuse.software/badge/github.com/abapPM/ABAP-Emoji)](https://api.reuse.software/info/github.com/abapPM/ABAP-Emoji)

# ✨ ABAP-Emoji ✨

GitHub and Twemoji Emoji Sets for ABAP.

- [GitHub Emoji](https://github.com/ikatyang/emoji-cheat-sheet/blob/master/README.md)
- [Twitter Emoji](https://github.com/twitter/twemoji) and [Twemoji Amazing](https://github.com/SebastianAigner/twemoji-amazing) (deprecated)

NO WARRANTIES, [MIT License](LICENSE)

> [!WARNING]
> Support for Twemoji is deprecated. It will be replaced by support for the full Unicode Emoji list in v2.

## Prerequisite

HTML output with Internet connection since Emoji graphics are hosted on https://twemoji.maxcdn.com/.

## Usage - GitHub Emoji

Use [GitHub Cheatsheet](https://github.com/ikatyang/emoji-cheat-sheet/blob/master/README.md) to view supported Emoji shortcodes. Raw text may contains the emoji character (cut&paste it into your text) or the emoji shortcode.

Get CSS for emoji class:

```abap
data(emoji) = zcl_emoji=>create( ).
data(css) = emoji->get_emoji_styles( ).
```

Find emojis with regex:

```abap
data(list) = emoji->find_emoji( '^heart$' ).
```

Format any text:

```abap
write emoji->format_emoji( 'I :heart: ABAP' ).
```

I ❤ ABAP

```html
I <img src="https://github.githubassets.com/images/icons/emoji/unicode/2764.png" class="emoji"> ABAP
```

## Usage - Twemoji

Use [Twemoji Cheatsheet](https://twemoji-cheatsheet.vercel.app/) to view supported Emoji.

To find the name of an Emoji, go to the official [Unicode Emoji List](https://unicode.org/emoji/charts/emoji-list.html). The name is based on the "CLDR short name" with spaces replaced by `-`.

Other helpful sources: [Emoji Test (Plain Text List)](https://unicode.org/Public/emoji/13.1/emoji-test.txt), [Emoji JSON](https://github.com/amio/emoji.json),
[Emoji Community Projects](https://github.com/twitter/twemoji#community-projects).

Get CSS for emoji class:

```abap
data(emoji) = zcl_emoji=>create( ).
data(css) = emoji->get_twemoji_styles( ).
```

Find twemojis with regex:

```abap
data(list) = emoji->find_twemoji( '^sparkles$' ).
```

Format any text:

```abap
write emoji->format_twemoji( 'I :red-heart: ABAP' ).
```

I ❤ ABAP

```html
I <i class="twa twa-red-heart"></i> ABAP
```

## Integrate with abapGit (Developer Version)

![image](https://github.com/abapPM/ABAP-Emoji/blob/main/img/abapGit_Emoji_Example.png?raw=true)

1. Insert one line into the following class

```abap
CLASS zcl_abapgit_syntax_highlighter IMPLEMENTATION.
...
  METHOD apply_style.
...
    lv_escaped = show_hidden_chars( lv_escaped ).

    lv_escaped = zcl_emoji=>create( )->format_emoji( lv_escaped ). "<<< insert
...
  ENDMETHOD.
```

2. Start transaction `SMW0` > `Binary data` > `$ABAPGIT`
3. Edit `ZABAPGIT_ICON_FONT_CSS`
4. Append [`twemoji-amazing.css`](https://github.com/mbtools/ABAP-Emoji/blob/main/css/twemoji-amazing.css) to the icon css and save

## Installation

Install `emoji` as a global module in your system using [apm](https://abappm.com).

or

Specify the `emoji` module as a dependency in your project and import it to your namespace using [apm](https://abappm.com).

## Contributions

All contributions are welcome! Read our [Contribution Guidelines](https://github.com/abapPM/ABAP-Emoji/blob/main/CONTRIBUTING.md), fork this repo, and create a pull request.

You can install the developer version of ABAP Emoji using [abapGit](https://github.com/abapGit/abapGit) by creating a new online repository for `https://github.com/abapPM/ABAP-Emoji`.

Recommended SAP package: `$EMOJI`

## Attribution

The Emoji data is under the Unicode License and copyright to the Unicode Consortium. The full license can be found here: http://www.unicode.org/copyright.html.

## About

Made with ❤ in Canada

Copyright 2025 apm.to Inc. <https://apm.to>

Follow [@marcf.be](https://bsky.app/profile/marcf.be) on Blueksy and [@marcfbe](https://linkedin.com/in/marcfbe) or LinkedIn
