********************************************************************************
* Emoji List
*
* Copyright 2026 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
********************************************************************************
* Check https://gist.github.com/rxaviers/7360908 for a nicely formatted list
********************************************************************************

REPORT /apmg/emoji_list.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  PARAMETERS p_regex TYPE string LOWER CASE DEFAULT 'arrow'.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  DATA(emoji) = /apmg/cl_emoji=>create( ).

  DATA(html) =
    `<html>` &&
    `<head>` &&
    `<title>Emoji Tester</title>` &&
    `<style>` && /apmg/cl_emoji=>styles( ) && `</style>` &&
    `</head>` &&
    `<body>`.

  DATA(list) = emoji->get_list( ).

  html = html && |<h1>Emoji List ({ lines( list ) } emoji)</h1>|.

  DATA(count) = 0.

  " TODO: Format this as a nice table
  LOOP AT list ASSIGNING FIELD-SYMBOL(<emoji>).
    IF p_regex IS NOT INITIAL.
      FIND REGEX p_regex IN <emoji> ##REGEX_POSIX.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    DATA(tag) = |:{ <emoji> }:|.
    html = html && emoji->format( tag ) && |  { tag }<br>|.
    count = count + 1.
  ENDLOOP.

  html = html && |<h2>{ count } emoji selected</h2>|.

  html = html && `</html>`.

  cl_abap_browser=>show_html(
    title       = 'Emoji List'
    dialog      = abap_false
    html_string = html ).
