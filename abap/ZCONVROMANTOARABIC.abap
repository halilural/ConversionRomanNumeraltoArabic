REPORT zconvromantoarabic.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS : p_val TYPE c LENGTH 20.

SELECTION-SCREEN END OF BLOCK b1.


INITIALIZATION.

CLASS lcl_conv_roman_exception DEFINITION INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    INTERFACES if_t100_message.

    ALIASES t100key FOR if_t100_message~t100key.

    METHODS : constructor IMPORTING id    TYPE sy-msgid
                                    no    TYPE sy-msgno
                                    text1 TYPE csequence OPTIONAL
                                    text2 TYPE csequence OPTIONAL
                                    text3 TYPE csequence OPTIONAL
                                    text4 TYPE csequence OPTIONAL.

    DATA text1 TYPE c LENGTH 50.
    DATA text2 TYPE c LENGTH 50.
    DATA text3 TYPE c LENGTH 50.
    DATA text4 TYPE c LENGTH 50.


ENDCLASS.

CLASS lcl_conv_roman_exception IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).
    me->text1 = text1.
    me->text2 = text2.
    me->text3 = text3.
    me->text4 = text4.
    t100key-msgid = id.
    t100key-msgno = no.
    t100key-attr1 = 'TEXT1'.
    t100key-attr2 = 'TEXT2'.
    t100key-attr3 = 'TEXT3'.
    t100key-attr4 = 'TEXT4'.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_roman_numeral  DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES : BEGIN OF ty_roman_numeral,
              roman_letter TYPE char1,
              arabic_num   TYPE int4,
            END OF ty_roman_numeral,

            tt_roman_numeral TYPE STANDARD TABLE OF ty_roman_numeral WITH DEFAULT KEY.

    METHODS :constructor IMPORTING iv_arabic TYPE int4 OPTIONAL
                                   iv_roman  TYPE string OPTIONAL
                         RAISING   lcl_conv_roman_exception,
      do_checks IMPORTING iv_arabic TYPE int4 OPTIONAL
                          iv_roman  TYPE string OPTIONAL
                RAISING   lcl_conv_roman_exception,
      letter_to_numbers IMPORTING iv_char       TYPE char1
                        RETURNING VALUE(re_num) TYPE int4,
      to_int RETURNING VALUE(re_num) TYPE int4,
      to_string RETURNING VALUE(re_string) TYPE string.
  PRIVATE SECTION.

    DATA mt_list TYPE tt_roman_numeral.
    DATA mv_num TYPE int4.

ENDCLASS.


CLASS lcl_roman_numeral IMPLEMENTATION.

  METHOD constructor.

    DATA lt_list TYPE tt_roman_numeral.
    DATA lv_roman TYPE string.

    IF iv_arabic IS SUPPLIED AND iv_roman IS SUPPLIED.

      RAISE EXCEPTION TYPE lcl_conv_roman_exception
        EXPORTING
          id    = 'ZMSG'
          no    = '01'
          text1 = 'Just choose one parameter at the same time!'.
    ENDIF.

    IF iv_arabic IS SUPPLIED.

      me->do_checks(
     EXPORTING
       iv_arabic                = iv_arabic
   ).

    ELSE.

      me->do_checks(
    EXPORTING
      iv_roman                 = iv_roman
  ).

    ENDIF.

    IF lines( me->mt_list ) EQ 0.

      lt_list = VALUE tt_roman_numeral(
   ( roman_letter = 'M'  arabic_num   = 1000 )
   ( roman_letter = 'CM'  arabic_num   = 900 )
   ( roman_letter = 'D'  arabic_num   = 500 )
   ( roman_letter = 'CD'  arabic_num   = 400 )
   ( roman_letter = 'C'  arabic_num   = 100 )
   ( roman_letter = 'XC'  arabic_num   = 90 )
   ( roman_letter = 'L'  arabic_num   = 50 )
   ( roman_letter = 'XL'  arabic_num   = 40 )
   ( roman_letter = 'X'  arabic_num   = 10 )
   ( roman_letter = 'IX'  arabic_num   = 9 )
   ( roman_letter = 'V'  arabic_num   = 5 )
   ( roman_letter = 'IV'  arabic_num   = 4 )
   ( roman_letter = 'I'  arabic_num   = 1 ) ).

      MOVE-CORRESPONDING lt_list TO mt_list.

    ENDIF.


    IF iv_arabic IS SUPPLIED.

      me->mv_num = iv_arabic.

    ELSE.

      " Convert roman numeral to arabic

      lv_roman = iv_roman.

      TRANSLATE lv_roman TO UPPER CASE.

      DATA(lv_i) = VALUE int4( ).
      DATA(lv_arabic) = VALUE int4( ).

      WHILE lv_i LT strlen( lv_roman ).

        DATA(lv_letter) = VALUE char1( ).
        CLEAR lv_letter.
        lv_letter = lv_roman+lv_i(1).
        DATA(lv_number) = me->letter_to_numbers( iv_char = lv_letter ).

        IF lv_number LT 0.
          RAISE EXCEPTION TYPE lcl_conv_roman_exception
            EXPORTING
              id    = 'ZMSG'
              no    = '01'
              text1 = |{ 'Illgeal character' }| && lv_letter && |{ 'in normal numeral' }|.
        ENDIF.

        ADD 1 TO lv_i.

        IF lv_i EQ strlen( lv_roman ).
          ADD lv_number TO lv_arabic.
        ELSE.

          lv_letter = lv_roman+lv_i(1).

          DATA(lv_nextnum) = me->letter_to_numbers( iv_char = lv_letter ).

          IF lv_nextnum GT lv_number.
            lv_arabic = lv_arabic + ( lv_nextnum - lv_number ).
            ADD 1 TO lv_i.
          ELSE.
            ADD lv_number TO lv_arabic.
          ENDIF.

        ENDIF.

      ENDWHILE.

      IF lv_arabic GT 3999.

        RAISE EXCEPTION TYPE lcl_conv_roman_exception
          EXPORTING
            id    = 'ZMSG'
            no    = '01'
            text1 = 'Value of RomanNumeral must be 3999 or less'.

      ENDIF.

      me->mv_num = lv_arabic.

    ENDIF.

  ENDMETHOD.

  METHOD do_checks.

    IF iv_arabic IS SUPPLIED.

      IF iv_arabic LT 1.
        RAISE EXCEPTION TYPE lcl_conv_roman_exception
          EXPORTING
            id    = 'ZMSG'
            no    = '01'
            text1 = 'Value of RomanNumeral must be positive'.
      ELSEIF iv_arabic GT 3999.
        RAISE EXCEPTION TYPE lcl_conv_roman_exception
          EXPORTING
            id    = 'ZMSG'
            no    = '01'
            text1 = 'Value of RomanNumeral must be 3999 or less'.
      ENDIF.

    ELSEIF iv_roman IS SUPPLIED.

      IF strlen( iv_roman ) EQ 0.

        RAISE EXCEPTION TYPE lcl_conv_roman_exception
          EXPORTING
            id    = 'ZMSG'
            no    = '01'
            text1 = 'An empty string does not define a roman numeral'.

      ENDIF.

    ENDIF.

  ENDMETHOD.

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

  METHOD to_int.
    re_num = me->mv_num.
  ENDMETHOD.

  METHOD to_string.

    DATA lv_roman TYPE string.
    DATA lv_n TYPE int4.
    DATA lv_counter TYPE int4.

    lv_n = me->mv_num.

    WHILE lv_counter < lines( me->mt_list ).
      READ TABLE me->mt_list REFERENCE INTO DATA(lr_line) INDEX ( lv_counter + 1 ).
      WHILE lv_n >= lr_line->arabic_num.
        CONCATENATE lv_roman lr_line->roman_letter INTO lv_roman.
        SUBTRACT lr_line->arabic_num FROM lv_n.
      ENDWHILE.
      ADD 1 TO lv_counter.
    ENDWHILE.

    re_string = lv_roman.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  TRY.

      " Decide whether input is arabic or roman

      DATA lr_roman TYPE REF TO lcl_roman_numeral.
      DATA lv_htype TYPE dd01v-datatype.

      CALL FUNCTION 'NUMERIC_CHECK'
        EXPORTING
          string_in = p_val
        IMPORTING
          htype     = lv_htype.

      IF lv_htype EQ 'NUMC'.
        lr_roman = NEW lcl_roman_numeral(
            iv_arabic                = CONV #( p_val )
        ).
        WRITE : lr_roman->to_int( ) && ' = ' && lr_roman->to_string( ).
      ELSE.
        lr_roman = NEW lcl_roman_numeral(
            iv_roman                = CONV #( p_val )
        ).
        WRITE : lr_roman->to_string( ) && ' = ' && lr_roman->to_int( ).
      ENDIF.

    CATCH lcl_conv_roman_exception INTO DATA(lr_ref).
      WRITE : / lr_ref->get_longtext( ).
  ENDTRY.