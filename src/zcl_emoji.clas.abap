CLASS zcl_emoji DEFINITION PUBLIC CREATE PRIVATE.

************************************************************************
* ABAP Emoji
*
* Support for GitHub Emoji
* https://docs.github.com/en/rest/emojis/emojis
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '2.0.0' ##NEEDED.

    TYPES:
      ty_code    TYPE string_table,
      ty_results TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line,
      BEGIN OF ty_emoji,
        name TYPE string,
        img  TYPE string,
        code TYPE string,
      END OF ty_emoji,
      ty_emojis TYPE HASHED TABLE OF ty_emoji
        WITH UNIQUE KEY name
        WITH NON-UNIQUE SORTED KEY code COMPONENTS code.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO zcl_emoji.

    METHODS constructor.

    METHODS get_emoji_css
      IMPORTING
        size_in_px    TYPE i DEFAULT 20
      RETURNING
        VALUE(result) TYPE ty_code.

    METHODS get_emoji_list
      RETURNING
        VALUE(result) TYPE ty_code.

    METHODS find_emoji
      IMPORTING
        !regex        TYPE string
      RETURNING
        VALUE(result) TYPE ty_results.

    METHODS format_emoji
      IMPORTING
        !line         TYPE string
        !base_url     TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_base_url TYPE string VALUE 'https://github.githubassets.com/images/icons/emoji'.

    CLASS-DATA emoji TYPE REF TO zcl_emoji.

    DATA emojis TYPE ty_emojis.

    METHODS init_emoji_list.

ENDCLASS.



CLASS zcl_emoji IMPLEMENTATION.


  METHOD constructor.

    init_emoji_list( ).

  ENDMETHOD.


  METHOD create.

    IF emoji IS INITIAL.
      emoji = NEW #( ).
    ENDIF.

    result = emoji.

  ENDMETHOD.


  METHOD find_emoji.

    LOOP AT emojis ASSIGNING FIELD-SYMBOL(<emoji>).
      IF find(
           val   = <emoji>-name
           regex = regex
           case  = abap_false ) >= 0.
        INSERT <emoji>-name INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD format_emoji.

    result = line.

    CHECK result CA ':' OR result CS '&#x'.

    IF base_url IS INITIAL.
      DATA(base) = c_base_url.
    ELSE.
      base = base_url.
    ENDIF.
    IF substring( val = base off = strlen( base ) - 1 len = 1 ) <> '/'.
      base = base && '/'.
    ENDIF.

    LOOP AT emojis ASSIGNING FIELD-SYMBOL(<emoji>).
      DATA(html)  = |<img src="{ base }{ <emoji>-img }" class="emoji">|.

      DATA(emoji) = |:{ <emoji>-name }:|.
      REPLACE ALL OCCURRENCES OF emoji IN result WITH html.
      IF <emoji>-code IS NOT INITIAL.
        DATA(code) = |&#x{ <emoji>-code };|.
        REPLACE ALL OCCURRENCES OF code IN result WITH html.
        REPLACE ALL OCCURRENCES OF <emoji>-code IN result WITH html.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_emoji_css.
    INSERT `.emoji {` INTO TABLE result.
    INSERT `  display: inline-block;` INTO TABLE result.
    INSERT `  min-width: 1ch;` INTO TABLE result.
    INSERT `  font-family: "Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol";` INTO TABLE result.
    INSERT `  font-size: 1em;` INTO TABLE result.
    INSERT `  font-style: normal !important;` INTO TABLE result.
    INSERT `  font-weight: 400;` INTO TABLE result.
    INSERT |  height: { size_in_px }px;| INTO TABLE result.
    INSERT |  width: { size_in_px }px;| INTO TABLE result.
    INSERT `  line-height: 1;` INTO TABLE result.
    INSERT `  vertical-align: -0.1em;` INTO TABLE result.
    INSERT `}` INTO TABLE result.
  ENDMETHOD.


  METHOD get_emoji_list.

    LOOP AT emojis ASSIGNING FIELD-SYMBOL(<emoji>).
      INSERT <emoji>-name INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD init_emoji_list.

    emojis = lcl_github_emoji=>get( ).

  ENDMETHOD.
ENDCLASS.
