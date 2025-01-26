********************************************************************************
* ABAP Emoji Import
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
********************************************************************************

REPORT z_emoji_import.

* https://unicode.org/emoji/charts/emoji-list.html

" Convert data from the official Unicode emoji list
" https://raw.githubusercontent.com/unicode-org/cldr-json/refs/heads/main/
" cldr-json/cldr-annotations-full/annotations/en/annotations.json
* and
" https://raw.githubusercontent.com/unicode-org/cldr-json/refs/heads/main/
" cldr-json/cldr-annotations-derived-full/annotationsDerived/en/annotations.json

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS p_path1 TYPE string LOWER CASE DEFAULT 'C:\Temp\annotations.json'.
  PARAMETERS p_path2 TYPE string LOWER CASE DEFAULT 'C:\Temp\annotations-derived.json'.
SELECTION-SCREEN END OF BLOCK b1.

DATA list TYPE string_table.

START-OF-SELECTION.

  " Unicode: 1f469-200d-1f4bb
  " GH: unicode/1f469-1f4bb.png

  DATA(count) = 0.

  PERFORM annotations USING `/annotations/annotations` p_path1.
  PERFORM annotations USING `/annotationsDerived/annotations` p_path2.

  ULINE.
  WRITE: / count.

FORM annotations USING
  root TYPE string
  path TYPE string.

  TRY.
      DATA(xstr) = zcl_abapgit_ui_factory=>get_frontend_services( )->file_upload( path ).
      DATA(str)  = zcl_abapgit_convert=>xstring_to_string_utf8( xstr ).
      DATA(json) = zcl_ajson=>parse( str ).
    CATCH cx_root INTO DATA(error).
      MESSAGE error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

  LOOP AT json->members( root ) INTO DATA(emoji).
    TRY.
        DATA(utf16_xstr) = cl_binary_convert=>string_to_xstring_utf16be( emoji ).
        DATA(utf16) = CONV string( utf16_xstr ).
        DATA(unicode) = ``.
        PERFORM convert_utf16_to_unicode USING utf16 unicode.
      CATCH cx_root INTO error.
        MESSAGE error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    DATA(out) = |* { unicode }\|{ utf16 }|.
    IF utf16 = '002F'.
      " slash needs to be replaced by tab for ajson
      emoji = cl_abap_char_utilities=>horizontal_tab.
    ENDIF.

    LOOP AT json->members( |{ root }/{ emoji }/tts| ) INTO DATA(tts).
      DATA(shortcode) = json->get( |{ root }/{ emoji }/tts/{ tts }| ).
      REPLACE ALL OCCURRENCES OF `"` IN shortcode WITH ''.
      REPLACE ALL OCCURRENCES OF `(` IN shortcode WITH ''.
      REPLACE ALL OCCURRENCES OF `)` IN shortcode WITH ''.
      "REPLACE ALL OCCURRENCES OF `:` IN shortcode WITH ''.
      REPLACE ALL OCCURRENCES OF ` ` IN shortcode WITH '-'.
      out = out && '|' && to_lower( shortcode ).
    ENDLOOP.
    LOOP AT json->members( |{ root }/{ emoji }/default| ) INTO DATA(default).
      out = out && '|' && json->get( |{ root }/{ emoji }/default/{ default }| ).
    ENDLOOP.

    WRITE / out COLOR COL_NORMAL.
    INSERT out INTO TABLE list.
    count = count + 1.
  ENDLOOP.
ENDFORM.

FORM convert_utf16_to_unicode USING
  utf16   TYPE string
  unicode TYPE string.

  CONSTANTS:
    x10000 TYPE x LENGTH 4 VALUE '00010000',
    xffff  TYPE x LENGTH 2 VALUE 'FFFF',
    xdfff  TYPE x LENGTH 2 VALUE 'DFFF',
    xdc00  TYPE x LENGTH 2 VALUE 'DC00',
    xdbff  TYPE x LENGTH 2 VALUE 'DBFF',
    xd800  TYPE x LENGTH 2 VALUE 'D800',
    x0400  TYPE x LENGTH 2 VALUE '0400'.

  DATA:
    x2 TYPE x LENGTH 2,
    x4 TYPE x LENGTH 4.

  DATA(len)    = strlen( utf16 ).
  DATA(offset) = 0.
  DO.
    DATA(codepoint) = ``.
    DATA(i)         = 0.

    x2     = utf16+offset(4).
    offset = offset + 4.

    " Surrogate code?
    IF x2 BETWEEN xd800 AND xdfff.
      x4     = x2 && utf16+offset(4).
      offset = offset + 4.

      DATA(high_surr) = CONV i( x4+1(1) ) + CONV i( x4+0(1) ) * 256.
      DATA(low_surr)  = CONV i( x4+3(1) ) + CONV i( x4+2(1) ) * 256.

      IF high_surr BETWEEN xd800 AND xdbff AND low_surr BETWEEN xdc00 AND xdfff.
        i = ( high_surr - xd800 ) * x0400 + ( low_surr - xdc00 ) + x10000.
      ELSE.
        BREAK-POINT.
      ENDIF.
    ELSE.
      i = CONV i( x2+1(1) ) + CONV i( x2+0(1) ) * 256.
    ENDIF.

    IF i > xffff.
      x4        = i.
      codepoint = x4.
    ELSEIF i > 0.
      x2        = i.
      codepoint = x2.
    ENDIF.
    SHIFT codepoint LEFT DELETING LEADING '0'.
    IF codepoint = '200D'.
      unicode = unicode && '-'.
    ELSE.
      unicode = unicode && codepoint.
    ENDIF.
    IF ( codepoint = '2640' OR codepoint = '2642' OR " female/maie-sign
         codepoint = '2695' OR codepoint = '2696' OR " medical-symbol / balance-scale
         codepoint = '2708' OR codepoint = '2764' OR
         codepoint = '27A1' ) AND len > 4.
      unicode = unicode && 'FE0F'.
    ENDIF.

    IF offset >= len.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.
