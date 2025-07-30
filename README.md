![Version](https://img.shields.io/endpoint?url=https://shield.abappm.com/github/abapPM/ABAP-Emoji/src/%2523apmg%2523cl_emoji.clas.abap/c_version&label=Version&color=blue)

[![License](https://img.shields.io/github/license/abapPM/ABAP-Emoji?label=License&color=success)](LICENSE)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg?color=success)](https://github.com/abapPM/.github/blob/main/CODE_OF_CONDUCT.md)
[![REUSE Status](https://api.reuse.software/badge/github.com/abapPM/ABAP-Emoji)](https://api.reuse.software/info/github.com/abapPM/ABAP-Emoji)

# ✨ ABAP-Emoji ✨

[GitHub Emoji](https://github.com/ikatyang/emoji-cheat-sheet/blob/master/README.md) Sets for ABAP.

Based on [GitHub Emoji](https://docs.github.com/en/rest/emojis/emojis) and [API](https://api.github.com/emojis), v8, 2025-04-05.

NO WARRANTIES, [MIT License](LICENSE)

## Prerequisite

HTML output with Internet connection since Emoji graphics are hosted on https://github.githubassets.com.

## Usage

Use [GitHub Cheatsheet](https://github.com/ikatyang/emoji-cheat-sheet/blob/master/README.md) to view supported Emoji shortcodes. Raw text may contains the emoji character (cut&paste it into your text) or the emoji shortcode.

Get CSS for the emoji class:

```abap
data(emoji) = /apmg/cl_emoji=>create( ).
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

## Integrate with abapGit (Developer Version)

![image](https://github.com/abapPM/ABAP-Emoji/blob/main/img/abapGit_Emoji_Example.png?raw=true)

Insert one line into the following class

```abap
CLASS zcl_abapgit_syntax_highlighter IMPLEMENTATION.
...
  METHOD apply_style.
...
    lv_escaped = show_hidden_chars( lv_escaped ).

    lv_escaped = /apmg/cl_emoji=>create( )->format_emoji( lv_escaped ). "<<< insert
...
  ENDMETHOD.
```

## Installation

Install `emoji` as a global module in your system using [apm](https://abappm.com).

or

Specify the `emoji` module as a dependency in your project and import it to your namespace using [apm](https://abappm.com).

## Contributions

All contributions are welcome! Read our [Contribution Guidelines](https://github.com/abapPM/ABAP-Emoji/blob/main/CONTRIBUTING.md), fork this repo, and create a pull request.

You can install the developer version of ABAP Emoji using [abapGit](https://github.com/abapGit/abapGit) by creating a new online repository for `https://github.com/abapPM/ABAP-Emoji`.

Recommended SAP package: `/APMG/EMOJI`

## Attribution

The Emoji data is under the Unicode License and copyright to the Unicode Consortium. The full license can be found here: http://www.unicode.org/copyright.html.

## About

Made with ❤ in Canada

Copyright 2025 apm.to Inc. <https://apm.to>

Follow [@marcf.be](https://bsky.app/profile/marcf.be) on Bluesky and [@marcfbe](https://linkedin.com/in/marcfbe) or LinkedIn
