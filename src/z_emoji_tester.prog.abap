********************************************************************************
* ABAP Emoji Tester
*
* https://github.com/Marc-Bernard-Tools/ABAP-Emoji
*
* Copyright 2022 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
********************************************************************************
* This is some code which includes a couple of in-line :emoji:
* You can see the emoji in abapGit if you install this as proposed in the
* repository readme
********************************************************************************

REPORT z_emoji_tester.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS p_text TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  cl_abap_browser=>show_html( html_string = zcl_emoji=>create( )->format_emoji( p_text ) ).
