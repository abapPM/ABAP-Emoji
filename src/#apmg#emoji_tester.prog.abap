********************************************************************************
* Emoji Tester
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
********************************************************************************

REPORT /apmg/emoji_tester.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS p_text TYPE string LOWER CASE DEFAULT 'Emoji for ABAP made with :heart: in Canada :canada:'.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  DATA(emoji) = /apmg/cl_emoji=>create( ).

  DATA(html) =
    `<!DOCTYPE html>` &&
    `<html lang="en">` &&
    `<head>` &&
    `<title>Emoji Tester</title>` &&
    `<style>` && /apmg/cl_emoji=>styles( ) && `</style>` &&
    `</head>` &&
    `<body>` && emoji->format( p_text ) && `</body>` &&
    `</html>`.

  cl_abap_browser=>show_html(
    title       = 'Emoji Tester'
    html_string = html ).
