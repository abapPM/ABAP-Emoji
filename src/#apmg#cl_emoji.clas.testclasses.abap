************************************************************************
* ABAP Emoji
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* To find UTF16 values for uccp, check this site:
* https://www.coderstool.com/unicode-text-converter
************************************************************************

CLASS ltcl_emoji_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    CONSTANTS c_url TYPE string VALUE 'https://github.githubassets.com/images/icons/emoji/unicode'.

    DATA cut TYPE REF TO /apmg/cl_emoji.

    METHODS setup.
    METHODS emoji_search FOR TESTING.
    METHODS emoji_format FOR TESTING.
    METHODS emoji_format_other_base FOR TESTING.
    METHODS emoji_css FOR TESTING.
    METHODS emoji_list FOR TESTING.
    METHODS emoji_unicode_heart FOR TESTING.
    METHODS emoji_unicode_gemini FOR TESTING.
    METHODS emoji_unicode_ambulance FOR TESTING.
    METHODS emoji_unicode_greenland FOR TESTING.

ENDCLASS.

CLASS ltcl_emoji_test IMPLEMENTATION.

  METHOD setup.
    cut = /apmg/cl_emoji=>create( ).
  ENDMETHOD.

  METHOD emoji_search.
    DATA(emojis) = cut->search( '^heart$' ).

    cl_aunit_assert=>assert_equals(
      act = lines( emojis )
      exp = 1 ).
  ENDMETHOD.

  METHOD emoji_format.
    DATA(html) = cut->format( 'Here is a :heart:' ).
    DATA(exp) = |Here is a <img src="{ c_url }/2764.png" class="emoji" alt="heart">|.

    cl_aunit_assert=>assert_equals(
      act = html
      exp = exp ).
  ENDMETHOD.

  METHOD emoji_format_other_base.
    DATA(html) = cut->format(
      line     = 'Here is a :heart:'
      base_url = 'https://mydomain.com/emoji' ).

    DATA(exp) = |Here is a <img src="https://mydomain.com/emoji/unicode/2764.png" class="emoji" alt="heart">|.

    cl_aunit_assert=>assert_equals(
      act = html
      exp = exp ).
  ENDMETHOD.

  METHOD emoji_css.
    DATA(emoji) = /apmg/cl_emoji=>styles( ).

    cl_aunit_assert=>assert_not_initial( emoji ).
  ENDMETHOD.

  METHOD emoji_list.
    DATA(emoji) = cut->get_list( ).

    cl_aunit_assert=>assert_not_initial( emoji ).
  ENDMETHOD.

  METHOD emoji_unicode_heart.
    " heart (utf16: \u2764)
    DATA(html) = cut->format( cl_abap_conv_in_ce=>uccp( '2764' ) ).
    DATA(exp)  = |<img src="{ c_url }/2764.png" class="emoji" alt="heart">|.

    cl_aunit_assert=>assert_equals(
      act = html
      exp = exp ).
  ENDMETHOD.

  METHOD emoji_unicode_gemini.
    " gemini (utf16: \u264A)
    DATA(html) = cut->format( cl_abap_conv_in_ce=>uccp( '264A' ) ).
    DATA(exp)  = |<img src="{ c_url }/264a.png" class="emoji" alt="gemini">|.

    cl_aunit_assert=>assert_equals(
      act = html
      exp = exp ).
  ENDMETHOD.

  METHOD emoji_unicode_ambulance.
    " ambulance (utf16: \uD83D \uDE91)
    DATA(html) = cut->format( cl_abap_conv_in_ce=>uccp( 'D83D' ) && cl_abap_conv_in_ce=>uccp( 'DE91' ) ).
    DATA(exp)  = |<img src="{ c_url }/1f691.png" class="emoji" alt="ambulance">|.

    cl_aunit_assert=>assert_equals(
      act = html
      exp = exp ).
  ENDMETHOD.

  METHOD emoji_unicode_greenland.
    " greenland (utf16 	\uD83C \uDDEC \uD83C \uDDF1)
    DATA(html) = cut->format( cl_abap_conv_in_ce=>uccp( 'D83C' ) && cl_abap_conv_in_ce=>uccp( 'DDEC' )
                           && cl_abap_conv_in_ce=>uccp( 'D83C' ) && cl_abap_conv_in_ce=>uccp( 'DDF1' ) ).
    DATA(exp)  = |<img src="{ c_url }/1f1ec-1f1f1.png" class="emoji" alt="greenland">|.

    cl_aunit_assert=>assert_equals(
      act = html
      exp = exp ).
  ENDMETHOD.

ENDCLASS.
