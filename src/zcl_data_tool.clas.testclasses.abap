CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_data,
             number      TYPE i,
             name        TYPE string,
             value       TYPE nrreturn_dec,
             other_value TYPE nrreturn_dec,
           END OF ty_data,

           ty_category_range TYPE RANGE OF cccategory,
           ty_mandt_range    TYPE RANGE OF mandt.

    METHODS:
      int_table_to_range FOR TESTING,
      int8_table_to_range FOR TESTING,
      decfloat16_table_to_range FOR TESTING,
      decfloat34_table_to_range FOR TESTING,
      float_table_to_range FOR TESTING,
      packed_table_to_range FOR TESTING,
      string_table_to_range FOR TESTING,
      char_table_to_range FOR TESTING,
      date_table_to_range FOR TESTING,
      time_table_to_range FOR TESTING,
      num_table_to_range FOR TESTING,
      ddic_table_to_range FOR TESTING,
      struc_table_to_range FOR TESTING,
      struc_w_high_table_to_range FOR TESTING,
      struc_only_high_table_to_range FOR TESTING,
      raise_struc_missing_exc FOR TESTING,
      table_domain_name_to_range FOR TESTING,
      table_domain_ref_to_range FOR TESTING,
      fixed_domain_name_to_range FOR TESTING,
      fixed_domain_ref_to_range FOR TESTING,
      get_category_range IMPORTING i_sign         TYPE char1 DEFAULT 'I'
                                   i_option       TYPE char2 DEFAULT 'EQ'
                         RETURNING VALUE(r_range) TYPE ty_category_range,
      get_mandt_range IMPORTING i_sign         TYPE char1 DEFAULT 'I'
                                i_option       TYPE char2 DEFAULT 'EQ'
                      RETURNING VALUE(r_range) TYPE ty_mandt_range.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD int_table_to_range.
    TYPES: ty_expected_range TYPE RANGE OF i,
           ty_int_table      TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    DATA(int_table) = VALUE ty_int_table( ( 1 ) ( 2 ) ( 3 ) ).

    DATA(expected_range) = VALUE ty_expected_range(
      (
        sign = 'I'
        option = 'EQ'
        low = 1
      )
      (
        sign = 'I'
        option = 'EQ'
        low = 2
      )
      (
        sign = 'I'
        option = 'EQ'
        low = 3
      )
    ).

    DATA(ref_range) = zcl_data_tool=>convert_int_table_to_range( i_table = REF #( int_table ) ).
    ASSIGN ref_range->* TO FIELD-SYMBOL(<actual_range>).
    cl_abap_unit_assert=>assert_equals( act = <actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD float_table_to_range.
    TYPES: ty_expected_range TYPE RANGE OF f,
           ty_table          TYPE STANDARD TABLE OF f WITH EMPTY KEY.

    DATA(table) = VALUE ty_table( ( CONV #( '1.20' ) ) ( CONV #( '1.30' ) ) ( CONV #( '1.40' ) ) ).

    DATA(expected_range) = VALUE ty_expected_range(
      (
        sign = 'I'
        option = 'EQ'
        low = '1.20'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = '1.30'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = '1.40'
      )
    ).

    DATA(ref_range) = zcl_data_tool=>convert_int_table_to_range( i_table = REF #( table ) ).
    ASSIGN ref_range->* TO FIELD-SYMBOL(<actual_range>).
    cl_abap_unit_assert=>assert_equals( act = <actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD decfloat16_table_to_range.
    TYPES: ty_expected_range TYPE RANGE OF decfloat16,
           ty_table          TYPE STANDARD TABLE OF decfloat16 WITH EMPTY KEY.

    DATA(table) = VALUE ty_table( ( CONV #('1.20') ) ( CONV #('1.30') ) ( CONV #('1.40') ) ).

    DATA(expected_range) = VALUE ty_expected_range(
      (
        sign = 'I'
        option = 'EQ'
        low = '1.20'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = '1.30'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = '1.40'
      )
    ).

    DATA(ref_range) = zcl_data_tool=>convert_int_table_to_range( i_table = REF #( table ) ).
    ASSIGN ref_range->* TO FIELD-SYMBOL(<actual_range>).
    cl_abap_unit_assert=>assert_equals( act = <actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD char_table_to_range.
    TYPES:ty_char           TYPE c LENGTH 3,
          ty_expected_range TYPE RANGE OF ty_char,
          ty_table          TYPE STANDARD TABLE OF ty_char WITH EMPTY KEY.

    DATA(table) = VALUE ty_table( ( 'abc' ) ( 'cba' ) ( 'abc' ) ).

    DATA(expected_range) = VALUE ty_expected_range(
      (
        sign = 'I'
        option = 'EQ'
        low = 'abc'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = 'cba'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = 'abc'
      )
    ).

    DATA(ref_range) = zcl_data_tool=>convert_int_table_to_range( i_table = REF #( table ) ).
    ASSIGN ref_range->* TO FIELD-SYMBOL(<actual_range>).
    cl_abap_unit_assert=>assert_equals( act = <actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD decfloat34_table_to_range.
    TYPES: ty_expected_range TYPE RANGE OF decfloat34,
           ty_table          TYPE STANDARD TABLE OF decfloat34 WITH EMPTY KEY.

    DATA(table) = VALUE ty_table( ( CONV #('1.20') ) ( CONV #('1.30') ) ( CONV #('1.40') ) ).

    DATA(expected_range) = VALUE ty_expected_range(
      (
        sign = 'I'
        option = 'EQ'
        low = '1.20'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = '1.30'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = '1.40'
      )
    ).

    DATA(ref_range) = zcl_data_tool=>convert_int_table_to_range( i_table = REF #( table ) ).
    ASSIGN ref_range->* TO FIELD-SYMBOL(<actual_range>).
    cl_abap_unit_assert=>assert_equals( act = <actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD packed_table_to_range.
    TYPES: ty_packed         TYPE p LENGTH 3 DECIMALS 2,
           ty_table          TYPE STANDARD TABLE OF ty_packed WITH DEFAULT KEY,
           ty_expected_range TYPE RANGE OF ty_packed.

    DATA(table) = VALUE ty_table( ( CONV #('111.20') ) ( CONV #('111.30') ) ( CONV #('111.40') ) ).

    DATA(expected_range) = VALUE ty_expected_range(
      (
        sign = 'I'
        option = 'EQ'
        low = '111.20'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = '111.30'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = '111.40'
      )
    ).

    DATA(ref_range) = zcl_data_tool=>convert_int_table_to_range( i_table = REF #( table ) ).
    ASSIGN ref_range->* TO FIELD-SYMBOL(<actual_range>).
    cl_abap_unit_assert=>assert_equals( act = <actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD string_table_to_range.
    TYPES: ty_expected_range TYPE RANGE OF string,
           ty_table          TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DATA(table) = VALUE ty_table( ( CONV #('abcc') ) ( CONV #('cbaa') ) ( CONV #('abcc') ) ).

    DATA(expected_range) = VALUE ty_expected_range(
      (
        sign = 'I'
        option = 'EQ'
        low = 'abcc'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = 'cbaa'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = 'abcc'
      )
    ).

    DATA(ref_range) = zcl_data_tool=>convert_int_table_to_range( i_table = REF #( table ) ).
    ASSIGN ref_range->* TO FIELD-SYMBOL(<actual_range>).
    cl_abap_unit_assert=>assert_equals( act = <actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD num_table_to_range.
    TYPES: ty_num            TYPE  n LENGTH 3,
           ty_table          TYPE STANDARD TABLE OF ty_num WITH DEFAULT KEY,
           ty_expected_range TYPE RANGE OF ty_num.

    DATA(table) = VALUE ty_table( ( CONV #('111') ) ( CONV #('112') ) ( CONV #('113') ) ).

    DATA(expected_range) = VALUE ty_expected_range(
      (
        sign = 'I'
        option = 'EQ'
        low = '111'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = '112'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = '113'
      )
    ).

    DATA(ref_range) = zcl_data_tool=>convert_int_table_to_range( i_table = REF #( table ) ).
    ASSIGN ref_range->* TO FIELD-SYMBOL(<actual_range>).
    cl_abap_unit_assert=>assert_equals( act = <actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD int8_table_to_range.
    TYPES: ty_expected_range TYPE RANGE OF int8,
           ty_int_table      TYPE STANDARD TABLE OF int8 WITH DEFAULT KEY.

    DATA(int_table) = VALUE ty_int_table( ( CONV #( 1 ) ) ( CONV #( 2 ) ) ( CONV #( 3 ) ) ).

    DATA(expected_range) = VALUE ty_expected_range(
      (
        sign = 'I'
        option = 'EQ'
        low = 1
      )
      (
        sign = 'I'
        option = 'EQ'
        low = 2
      )
      (
        sign = 'I'
        option = 'EQ'
        low = 3
      )
    ).

    DATA(ref_range) = zcl_data_tool=>convert_int_table_to_range( i_table = REF #( int_table ) ).
    ASSIGN ref_range->* TO FIELD-SYMBOL(<actual_range>).
    cl_abap_unit_assert=>assert_equals( act = <actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD date_table_to_range.
    TYPES: ty_expected_range TYPE RANGE OF d,
           ty_table          TYPE STANDARD TABLE OF d WITH DEFAULT KEY.

    DATA(table) = VALUE ty_table( ( CONV #( '20190101' ) )  ( CONV #( '20190102' ) ) ( CONV #( '20190103' ) ) ).

    DATA(expected_range) = VALUE ty_expected_range(
      (
        sign = 'I'
        option = 'EQ'
        low = '20190101'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = '20190102'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = '20190103'
      )
    ).

    DATA(ref_range) = zcl_data_tool=>convert_int_table_to_range( i_table = REF #( table ) ).
    ASSIGN ref_range->* TO FIELD-SYMBOL(<actual_range>).
    cl_abap_unit_assert=>assert_equals( act = <actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD time_table_to_range.
    TYPES: ty_expected_range TYPE RANGE OF t,
           ty_table          TYPE STANDARD TABLE OF t WITH DEFAULT KEY.

    DATA(table) = VALUE ty_table( ( CONV #( '165001' ) )  ( CONV #( '165101' ) ) ( CONV #( '165201' ) ) ).

    DATA(expected_range) = VALUE ty_expected_range(
      (
        sign = 'I'
        option = 'EQ'
        low = '165001'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = '165101'
      )
      (
        sign = 'I'
        option = 'EQ'
        low = '165201'
      )
    ).

    DATA(ref_range) = zcl_data_tool=>convert_int_table_to_range( i_table = REF #( table ) ).
    ASSIGN ref_range->* TO FIELD-SYMBOL(<actual_range>).
    cl_abap_unit_assert=>assert_equals( act = <actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD ddic_table_to_range.
    TYPES: ty_table          TYPE STANDARD TABLE OF bapibp_curr WITH DEFAULT KEY,
           ty_expected_range TYPE RANGE OF bapibp_curr.

    DATA(table) = VALUE ty_table( ( CONV #( 'AAAA' ) )  ( CONV #( 'BBBB' ) ) ( CONV #( 'CCCC' ) ) ).

    DATA(expected_range) = VALUE ty_expected_range(
     (
       sign = 'I'
       option = 'EQ'
       low = 'AAAA'
     )
     (
       sign = 'I'
       option = 'EQ'
       low = 'BBBB'
     )
     (
       sign = 'I'
       option = 'EQ'
       low = 'CCCC'
     )
   ).

    DATA(ref_range) = zcl_data_tool=>convert_int_table_to_range( i_table = REF #( table ) ).
    ASSIGN ref_range->* TO FIELD-SYMBOL(<actual_range>).
    cl_abap_unit_assert=>assert_equals( act = <actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD struc_table_to_range.
    TYPES: ty_struc_table    TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY,
           ty_expected_range TYPE RANGE OF string.

    DATA(table) = VALUE ty_struc_table(
      (
        number = 1
        name = 'abc'
        value = '20'
      )
      (
        number = 2
        name = 'cba'
        value = '30'
      )
    ).

    DATA(expected_range) = VALUE ty_expected_range(
      (
        sign = 'E'
        option = 'EQ'
        low = 'abc'
      )
      (
        sign = 'E'
        option = 'EQ'
        low = 'cba'
      )
    ).

    DATA(ref_range) = zcl_data_tool=>convert_int_table_to_range( i_table = REF #( table ) i_low_fieldname = 'NAME' i_sign = 'E' ).
    ASSIGN ref_range->* TO FIELD-SYMBOL(<actual_range>).
    cl_abap_unit_assert=>assert_equals( act = <actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD raise_struc_missing_exc.
    TYPES: ty_table TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY.

    DATA(table) = VALUE ty_table(
      (
        number = 1
        name = 'abc'
        value = '20'
      )
    ).

    TRY.
        zcl_data_tool=>convert_int_table_to_range( i_table = REF #( table ) i_sign = 'E' ).
      CATCH lcx_data_tool_exception INTO DATA(exception).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound( exception ).
  ENDMETHOD.

  METHOD struc_w_high_table_to_range.
    TYPES: ty_struc_table    TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY,
           ty_expected_range TYPE RANGE OF nrreturn_dec.

    DATA(table) = VALUE ty_struc_table(
      (
        number = 1
        name = 'abc'
        value = '20'
        other_value = '30'
      )
      (
        number = 2
        name = 'cba'
        value = '30'
        other_value = '40'
      )
    ).

    DATA(expected_range) = VALUE ty_expected_range(
      (
        sign = 'E'
        option = 'BT'
        low = '20'
        high = '30'
      )
      (
        sign = 'E'
        option = 'BT'
        low = '30'
        high = '40'
      )
    ).

    DATA(ref_range) = zcl_data_tool=>convert_int_table_to_range(
      i_table = REF #( table )
      i_low_fieldname = 'VALUE'
      i_high_fieldname = 'OTHER_VALUE'
      i_sign = 'E'
      i_option = 'BT'
    ).

    ASSIGN ref_range->* TO FIELD-SYMBOL(<actual_range>).
    cl_abap_unit_assert=>assert_equals( act = <actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD struc_only_high_table_to_range.
    TYPES: ty_struc_table    TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY,
           ty_expected_range TYPE RANGE OF nrreturn_dec.

    DATA(table) = VALUE ty_struc_table(
      (
        number = 1
        name = 'abc'
        value = '20'
        other_value = '30'
      )
      (
        number = 2
        name = 'cba'
        value = '30'
        other_value = '40'
      )
    ).

    DATA(expected_range) = VALUE ty_expected_range(
      (
        sign = 'E'
        option = 'BT'
        low = ''
        high = '30'
      )
      (
        sign = 'E'
        option = 'BT'
        low = ''
        high = '40'
      )
    ).

    DATA(ref_range) = zcl_data_tool=>convert_int_table_to_range(
      i_table = REF #( table )
      i_high_fieldname = 'OTHER_VALUE'
      i_sign = 'E'
      i_option = 'BT'
    ).

    ASSIGN ref_range->* TO FIELD-SYMBOL(<actual_range>).
    cl_abap_unit_assert=>assert_equals( act = <actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD fixed_domain_name_to_range.
    DATA(expected_range) = get_category_range(
      i_sign = 'E'
      i_option = 'GT'
    ).

    DATA(range) = zcl_data_tool=>convert_domain_to_range(
      i_sign = 'E'
      i_option = 'GT'
      i_domain_name = 'CCCATEGORY'
    ).

    cl_abap_unit_assert=>assert_bound( act = range ).
    ASSIGN range->* TO FIELD-SYMBOL(<range>).
    cl_abap_unit_assert=>assert_equals( act = <range> exp = expected_range ).
  ENDMETHOD.

  METHOD fixed_domain_ref_to_range.
    DATA: domain         TYPE cccategory.

    DATA(expected_range) = get_category_range(
      i_sign = 'E'
      i_option = 'GT'
    ).

    DATA(range) = zcl_data_tool=>convert_domain_to_range(
      i_sign = 'E'
      i_option = 'GT'
      i_domain_reference = REF #( domain )
    ).

    cl_abap_unit_assert=>assert_bound( act = range ).
    ASSIGN range->* TO FIELD-SYMBOL(<range>).
    cl_abap_unit_assert=>assert_equals( act = <range> exp = expected_range ).
  ENDMETHOD.

  METHOD table_domain_ref_to_range.
    DATA: domain         TYPE bapimandt.

    DATA(expected_range) = get_mandt_range( ).

    DATA(range) = zcl_data_tool=>convert_domain_to_range(
      i_sign = 'I'
      i_option = 'EQ'
      i_domain_reference = REF #( domain )
    ).

    cl_abap_unit_assert=>assert_bound( act = range ).
    ASSIGN range->* TO FIELD-SYMBOL(<range>).
    cl_abap_unit_assert=>assert_equals( act = <range> exp = expected_range ).
  ENDMETHOD.

  METHOD table_domain_name_to_range.
    DATA(expected_range) = get_mandt_range( ).

    DATA(range) = zcl_data_tool=>convert_domain_to_range(
      i_sign = 'I'
      i_option = 'EQ'
      i_domain_name = 'MANDT'
    ).

    cl_abap_unit_assert=>assert_bound( act = range ).
    ASSIGN range->* TO FIELD-SYMBOL(<range>).
    cl_abap_unit_assert=>assert_equals( act = <range> exp = expected_range ).
  ENDMETHOD.

  METHOD get_category_range.
    r_range = VALUE #(
      (
        sign = i_sign
        option = i_option
        low = 'P'
      )
      (
        sign = i_sign
        option = i_option
        low = 'T'
      )
      (
        sign = i_sign
        option = i_option
        low = 'C'
      )
      (
        sign = i_sign
        option = i_option
        low = 'D'
      )
      (
        sign = i_sign
        option = i_option
        low = 'E'
      )
      (
        sign = i_sign
        option = i_option
        low = 'S'
      )
    ).
  ENDMETHOD.

  METHOD get_mandt_range.
    r_range = VALUE #(
      (
        sign = i_sign
        option = i_option
        low = '000'
      )
      (
        sign = i_sign
        option = i_option
        low = '001'
      )
    ).
  ENDMETHOD.
ENDCLASS.
