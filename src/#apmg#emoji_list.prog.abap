********************************************************************************
* Emoji List
*
* Copyright 2026 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
********************************************************************************
* Check https://gist.github.com/rxaviers/7360908 for a nicely formatted list
********************************************************************************

REPORT /apmg/emoji_list.

START-OF-SELECTION.

  DATA(emoji) = /apmg/cl_emoji=>create( ).

  DATA(html) =
    `<html>` &&
    `<head>` &&
    `<title>Emoji Tester</title>` &&
    `<style>` && concat_lines_of( emoji->get_css( ) ) && `</style>` &&
    `</head>` &&
    `<body>`.

  DATA(list) = emoji->get_list( ).

  html = html && |<h1>Emoji List ({ lines( list ) } emoji)</h1>|.

  " TODO: Format this as a nice table
  LOOP AT list ASSIGNING FIELD-SYMBOL(<emoji>).
    data(tag) = |:{ <emoji> }:|.
    html = html && emoji->format( tag ) && |  { tag }<br>|.
  ENDLOOP.

  html = html && `</html>`.

  cl_abap_browser=>show_html(
    title       = 'Emoji List'
    dialog      = abap_false
    html_string = html ).
