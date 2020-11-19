class ZCL_ROMAN_CONVERSION definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF TY_ROMAN_NUMERAL,
         roman_letter TYPE char1,
         arabic_num TYPE int4,
         END OF TY_ROMAN_NUMERAL .
  types:
    tt_roman_numeral TYPE STANDARD TABLE OF TY_ROMAN_NUMERAL WITH DEFAULT KEY .

  methods CONSTRUCTOR
    importing
      value(IV_ARABIC) type INT4 optional
      value(IV_ROMAN) type STRING optional
    raising
      ZCX_ROMAN_EXCEPTION .
  methods TO_ARABIC
    returning
      value(RE_ARABIC) type INT4 .
  methods TO_ROMAN
    returning
      value(RE_ROMAN) type STRING .
protected section.
private section.

  data MT_LIST type TT_ROMAN_NUMERAL .
  data MV_ARABIC type INT4 .
  data MV_ROMAN type STRING .

  methods DO_CHECKS
    importing
      value(IV_ARABIC) type INT4 optional
      value(IV_ROMAN) type STRING optional
    raising
      ZCX_ROMAN_EXCEPTION .
  methods LETTER_TO_NUMBERS
    importing
      value(IV_CHAR) type CHAR1
    returning
      value(RE_NUM) type INT4 .
ENDCLASS.



CLASS ZCL_ROMAN_CONVERSION IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ROMAN_CONVERSION->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ARABIC                      TYPE        INT4(optional)
* | [--->] IV_ROMAN                       TYPE        STRING(optional)
* | [!CX!] ZCX_ROMAN_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    DATA lt_list TYPE tt_roman_numeral.
    DATA lv_roman TYPE string.

    IF iv_arabic IS SUPPLIED.

      me->do_checks(
        EXPORTING
          iv_arabic           = iv_arabic    " Doğal sayı
      ).

    ELSE.

      me->do_checks(
        EXPORTING
          iv_roman            = iv_roman
      ).

    ENDIF.

    IF lines( mt_list ) EQ 0.
      lt_list = VALUE tt_roman_numeral(
      ( roman_letter = 'M' arabic_num   = 1000 )
      ( roman_letter = 'CM' arabic_num   = 900 )
      ( roman_letter = 'D' arabic_num   = 500 )
      ( roman_letter = 'CD' arabic_num   = 400 )
      ( roman_letter = 'C' arabic_num   = 100 )
      ( roman_letter = 'XC' arabic_num   = 90 )
      ( roman_letter = 'L' arabic_num   = 50 )
      ( roman_letter = 'XL' arabic_num   = 40 )
      ( roman_letter = 'X' arabic_num   = 10 )
      ( roman_letter = 'IX' arabic_num   = 9 )
      ( roman_letter = 'V' arabic_num   = 5 )
      ( roman_letter = 'IV' arabic_num   = 4 )
      ( roman_letter = 'I' arabic_num   = 1 ) ).

      MOVE-CORRESPONDING lt_list TO mt_list.

    ENDIF.

    clear : me->mv_arabic , me->mv_roman.

    me->mv_arabic = iv_arabic.
    me->mv_roman = iv_roman.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ROMAN_CONVERSION->DO_CHECKS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ARABIC                      TYPE        INT4(optional)
* | [--->] IV_ROMAN                       TYPE        STRING(optional)
* | [!CX!] ZCX_ROMAN_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD do_checks.

    IF iv_arabic IS SUPPLIED.

      IF iv_arabic LT 1.
        RAISE EXCEPTION TYPE zcx_roman_exception
          EXPORTING
            textid = zcx_roman_exception=>roman_numeral_positive_exp.
      ELSEIF iv_arabic GT 3999.
        RAISE EXCEPTION TYPE zcx_roman_exception
          EXPORTING
            textid = zcx_roman_exception=>high_numeral_exception.
      ENDIF.
    ELSE.

      IF strlen( iv_roman ) EQ 0.
        RAISE EXCEPTION TYPE zcx_roman_exception
          EXPORTING
            textid = zcx_roman_exception=>emtpy_roman_text_exception.

      ENDIF.


    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ROMAN_CONVERSION->LETTER_TO_NUMBERS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CHAR                        TYPE        CHAR1
* | [<-()] RE_NUM                         TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD letter_to_numbers.

    CASE iv_char.
      WHEN 'I'.
        re_num = 1.
      WHEN 'V'.
        re_num = 5.
      WHEN 'X'.
        re_num = 10.
      WHEN 'L'.
        re_num = 50.
      WHEN 'C'.
        re_num = 100.
      WHEN 'D'.
        re_num = 500.
      WHEN 'M'.
        re_num = 1000.
      WHEN OTHERS.
        re_num = -1.
    ENDCASE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ROMAN_CONVERSION->TO_ARABIC
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RE_ARABIC                      TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD to_arabic.

    "Convert roman numeral to arabic one.

    data lv_roman TYPE string.

    lv_roman = me->mv_roman.

    TRANSLATE lv_roman TO UPPER CASE.

    DATA(lv_i) = VALUE int4( ).
    DATA(lv_arabic) = VALUE int4( ).

    WHILE lv_i LT strlen( lv_roman ).

      DATA(lv_letter) = VALUE char1( ).
      CLEAR lv_letter.
      lv_letter = lv_roman+lv_i(1).
      DATA(lv_number) = me->letter_to_numbers( iv_char = lv_letter ).

      IF lv_number EQ -1.
        RAISE EXCEPTION TYPE zcx_roman_exception
          EXPORTING
            textid = zcx_roman_exception=>illegal_character_exception.
      ENDIF.

      ADD 1 TO lv_i.

      IF lv_i EQ strlen( lv_roman ).
        ADD lv_number TO lv_arabic.
      ELSE.

        lv_letter = lv_roman+lv_i(1).

        DATA(lv_nextnum) = me->letter_to_numbers( iv_char = lv_letter ).

        IF lv_nextnum GT lv_number.

          lv_arabic = lv_arabic + ( lv_nextnum - lv_number ).
        ELSE.
          ADD lv_number TO lv_arabic.
        ENDIF.

      ENDIF.

    ENDWHILE.

    IF lv_arabic GT 3999.

      RAISE EXCEPTION TYPE zcx_roman_exception
        EXPORTING
          textid = zcx_roman_exception=>high_numeral_exception.

    ENDIF.

    re_arabic = lv_arabic.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ROMAN_CONVERSION->TO_ROMAN
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RE_ROMAN                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD to_roman.

    DATA lv_roman TYPE string.
    DATA lv_n TYPE int4.
    DATA lv_counter TYPE int4.

    lv_n = me->mv_arabic.

    WHILE lv_counter < lines( me->mt_list ).

      READ TABLE me->mt_list REFERENCE INTO DATA(lr_line)
      INDEX ( lv_counter + 1 ).

      WHILE lv_n >= lr_line->arabic_num.
        CONCATENATE lv_roman lr_line->roman_letter INTO lv_roman.
        SUBTRACT lr_line->arabic_num FROM lv_n.
      ENDWHILE.
      add 1 to lv_counter.
    ENDWHILE.

    re_roman = lv_roman.

  ENDMETHOD.
ENDCLASS.