
CLASS ltc_unit_test_roman_conversion DEFINITION FOR TESTING
DURATION SHORT
RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_Unit_Test_Roman_Conversion
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_ROMAN_CONVERSION
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
PRIVATE SECTION.
  DATA:
    f_cut TYPE REF TO zcl_roman_conversion.  "class under test

  CLASS-METHODS: class_setup.
  CLASS-METHODS: class_teardown.
  METHODS: setup.
  METHODS: teardown.
  METHODS: to_arabic FOR TESTING.
  METHODS: to_roman FOR TESTING.
ENDCLASS.       "ltc_Unit_Test_Roman_Conversion


CLASS ltc_unit_test_roman_conversion IMPLEMENTATION.

METHOD class_setup.



ENDMETHOD.


METHOD class_teardown.



ENDMETHOD.


METHOD setup.

  DATA iv_arabic TYPE int4.
  DATA iv_roman TYPE string.

  iv_arabic = 1156.
  iv_roman = 'MCLVI'.

  CREATE OBJECT f_cut
    EXPORTING
      iv_arabic = iv_arabic
      iv_roman  = iv_roman.
ENDMETHOD.


METHOD teardown.



ENDMETHOD.


METHOD to_arabic.

  DATA re_arabic TYPE int4.

  re_arabic = f_cut->to_arabic(  ).

  cl_abap_unit_assert=>assert_equals(
    act   = re_arabic
    exp   = 1156          "<--- please adapt expected value
   msg   = 'Testing value MCLVI'
*     level =
  ).
ENDMETHOD.


METHOD to_roman.

  DATA re_roman TYPE string.

  re_roman = f_cut->to_roman(  ).

  cl_abap_unit_assert=>assert_equals(
    act   = re_roman
    exp   = 'MCLVI'          "<--- please adapt expected value
   msg   = 'Testing value 1156'
  ).
ENDMETHOD.




ENDCLASS.