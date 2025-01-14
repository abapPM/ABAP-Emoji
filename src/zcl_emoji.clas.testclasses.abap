********************************************************************************
* ABAP Emoji
*
* Copyright 2022 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
********************************************************************************
CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_emoji.

    METHODS setup.
    METHODS emoji_find FOR TESTING.
    METHODS emoji_format FOR TESTING.
    METHODS emoji_format_other_base FOR TESTING.
    METHODS emoji_css FOR TESTING.
    METHODS emoji_list FOR TESTING.
    METHODS twemoji_find FOR TESTING.
    METHODS twemoji_format FOR TESTING.
    METHODS twemoji_css FOR TESTING.
    METHODS twemoji_list FOR TESTING.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    cut = zcl_emoji=>create( ).
  ENDMETHOD.

  METHOD emoji_find.
    DATA(emoji) = cut->find_emoji( '^heart$' ).

    cl_aunit_assert=>assert_equals(
      act = lines( emoji )
      exp = 1 ).
  ENDMETHOD.

  METHOD emoji_format.
    DATA(html) = cut->format_emoji( 'Here is a :heart:' ).
    DATA(exp) = 'Here is a <img src="https://github.githubassets.com/images/icons/emoji/unicode/2764.png" class="emoji">'.

    cl_aunit_assert=>assert_equals(
      act = html
      exp = exp ).
  ENDMETHOD.

  METHOD emoji_format_other_base.
    DATA(html) = cut->format_emoji(
      line     = 'Here is a :heart:'
      base_url = 'https://mydomain.com/emoji/' ).

    DATA(exp) = 'Here is a <img src="https://mydomain.com/emoji/unicode/2764.png" class="emoji">'.

    cl_aunit_assert=>assert_equals(
      act = html
      exp = exp ).
  ENDMETHOD.

  METHOD emoji_css.
    DATA(emoji) = cut->get_emoji_css( ).

    cl_aunit_assert=>assert_not_initial( emoji ).
  ENDMETHOD.

  METHOD emoji_list.
    DATA(emoji) = cut->get_emoji_list( ).

    cl_aunit_assert=>assert_not_initial( emoji ).
  ENDMETHOD.

  METHOD twemoji_find.
    DATA(emoji) = cut->find_twemoji( '^sparkles$' ).

    cl_aunit_assert=>assert_equals(
      act = lines( emoji )
      exp = 1 ).
  ENDMETHOD.

  METHOD twemoji_format.
    DATA(html) = cut->format_twemoji( 'Here are some :sparkles:' ).

    cl_aunit_assert=>assert_equals(
      act = html
      exp = 'Here are some <i class="twa twa-sparkles"></i>' ).
  ENDMETHOD.

  METHOD twemoji_css.
    DATA(emoji) = cut->get_twemoji_css( ).

    cl_aunit_assert=>assert_not_initial( emoji ).
  ENDMETHOD.

  METHOD twemoji_list.
    DATA(emoji) = cut->get_twemoji_list( ).

    cl_aunit_assert=>assert_not_initial( emoji ).
  ENDMETHOD.

ENDCLASS.
