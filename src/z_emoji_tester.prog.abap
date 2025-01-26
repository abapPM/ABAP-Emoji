********************************************************************************
* ABAP Emoji Tester
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
********************************************************************************

REPORT z_emoji_tester.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS p_text TYPE string LOWER CASE DEFAULT 'Emoji for ABAP made with :heart: in Canada :canada:'.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  DATA(emoji) = zcl_emoji=>create( ).
  cl_abap_browser=>show_html( html_string = emoji->format_emoji( p_text ) ).
