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
    DATA: int_table           TYPE STANDARD TABLE OF i,
          ref_table           TYPE REF TO data,
          ref_range           TYPE REF TO data,
          expected_range      TYPE RANGE OF i,
          expected_range_line LIKE LINE OF expected_range.

    FIELD-SYMBOLS: <fs_actual_range> LIKE expected_range.

    APPEND 1 TO int_table.
    APPEND 2 TO int_table.
    APPEND 3 TO int_table.

    expected_range_line-sign = 'I'.
    expected_range_line-option = 'EQ'.
    expected_range_line-low = 1.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = 2.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = 3.
    APPEND expected_range_line TO expected_range.

    GET REFERENCE OF int_table INTO ref_table.
    ref_range = zcl_data_tool_pre74=>convert_int_table_to_range( i_table = ref_table ).
    ASSIGN ref_range->* TO <fs_actual_range>.

    cl_abap_unit_assert=>assert_equals( act = <fs_actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD float_table_to_range.
    DATA: float_table         TYPE STANDARD TABLE OF f,
          ref_table           TYPE REF TO data,
          ref_range           TYPE REF TO data,
          expected_range      TYPE RANGE OF f,
          expected_range_line LIKE LINE OF expected_range.

    FIELD-SYMBOLS: <fs_actual_range> LIKE expected_range.

    APPEND '1.40' TO float_table.
    APPEND '1.50' TO float_table.
    APPEND '1.60' TO float_table.

    expected_range_line-sign = 'I'.
    expected_range_line-option = 'EQ'.
    expected_range_line-low = '1.40'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low ='1.50'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = '1.60'.
    APPEND expected_range_line TO expected_range.

    GET REFERENCE OF float_table INTO ref_table.
    ref_range = zcl_data_tool_pre74=>convert_int_table_to_range( i_table = ref_table ).
    ASSIGN ref_range->* TO <fs_actual_range>.

    cl_abap_unit_assert=>assert_equals( act = <fs_actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD decfloat16_table_to_range.
    DATA: float_table         TYPE STANDARD TABLE OF decfloat16,
          ref_table           TYPE REF TO data,
          ref_range           TYPE REF TO data,
          expected_range      TYPE RANGE OF decfloat16,
          expected_range_line LIKE LINE OF expected_range.

    FIELD-SYMBOLS: <fs_actual_range> LIKE expected_range.

    APPEND '1.30' TO float_table.
    APPEND '1.40' TO float_table.
    APPEND '1.50' TO float_table.

    expected_range_line-sign = 'I'.
    expected_range_line-option = 'EQ'.
    expected_range_line-low = '1.30'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = '1.40'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = '1.50'.
    APPEND expected_range_line TO expected_range.

    GET REFERENCE OF float_table INTO ref_table.
    ref_range = zcl_data_tool_pre74=>convert_int_table_to_range( i_table = ref_table ).
    ASSIGN ref_range->* TO <fs_actual_range>.

    cl_abap_unit_assert=>assert_equals( act = <fs_actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD char_table_to_range.
    TYPES: ty_char TYPE c LENGTH 3.
    DATA: char_table          TYPE STANDARD TABLE OF ty_char,
          ref_table           TYPE REF TO data,
          ref_range           TYPE REF TO data,
          expected_range      TYPE RANGE OF ty_char,
          expected_range_line LIKE LINE OF expected_range.

    FIELD-SYMBOLS: <fs_actual_range> LIKE expected_range.

    APPEND 'abc' TO char_table.
    APPEND 'cba' TO char_table.
    APPEND 'abc' TO char_table.

    expected_range_line-sign = 'E'.
    expected_range_line-option = 'EQ'.
    expected_range_line-low = 'abc'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = 'cba'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = 'abc'.
    APPEND expected_range_line TO expected_range.

    GET REFERENCE OF char_table INTO ref_table.
    ref_range = zcl_data_tool_pre74=>convert_int_table_to_range( i_table = ref_table i_sign = 'E' ).
    ASSIGN ref_range->* TO <fs_actual_range>.

    cl_abap_unit_assert=>assert_equals( act = <fs_actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD decfloat34_table_to_range.
    DATA: float_table         TYPE STANDARD TABLE OF decfloat34,
          ref_table           TYPE REF TO data,
          ref_range           TYPE REF TO data,
          expected_range      TYPE RANGE OF decfloat34,
          expected_range_line LIKE LINE OF expected_range.

    FIELD-SYMBOLS: <fs_actual_range> LIKE expected_range.

    APPEND '1.30' TO float_table.
    APPEND '1.40' TO float_table.
    APPEND '1.50' TO float_table.

    expected_range_line-sign = 'I'.
    expected_range_line-option = 'EQ'.
    expected_range_line-low = '1.30'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = '1.40'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = '1.50'.
    APPEND expected_range_line TO expected_range.

    GET REFERENCE OF float_table INTO ref_table.
    ref_range = zcl_data_tool_pre74=>convert_int_table_to_range( i_table = ref_table ).
    ASSIGN ref_range->* TO <fs_actual_range>.

    cl_abap_unit_assert=>assert_equals( act = <fs_actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD packed_table_to_range.
    TYPES: ty_packed TYPE p LENGTH 3 DECIMALS 2.

    DATA: packed_table        TYPE STANDARD TABLE OF ty_packed,
          ref_table           TYPE REF TO data,
          ref_range           TYPE REF TO data,
          expected_range      TYPE RANGE OF ty_packed,
          expected_range_line LIKE LINE OF expected_range.

    FIELD-SYMBOLS: <fs_actual_range> LIKE expected_range.

    APPEND '111.31' TO packed_table.
    APPEND '111.42' TO packed_table.
    APPEND '111.53' TO packed_table.

    expected_range_line-sign = 'I'.
    expected_range_line-option = 'EQ'.
    expected_range_line-low = '111.31'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = '111.42'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = '111.53'.
    APPEND expected_range_line TO expected_range.

    GET REFERENCE OF packed_table INTO ref_table.
    ref_range = zcl_data_tool_pre74=>convert_int_table_to_range( i_table = ref_table ).
    ASSIGN ref_range->* TO <fs_actual_range>.

    cl_abap_unit_assert=>assert_equals( act = <fs_actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD string_table_to_range.
    DATA: string_table        TYPE STANDARD TABLE OF string,
          ref_table           TYPE REF TO data,
          ref_range           TYPE REF TO data,
          expected_range      TYPE RANGE OF string,
          expected_range_line LIKE LINE OF expected_range.

    FIELD-SYMBOLS: <fs_actual_range> LIKE expected_range.

    APPEND 'aaaa' TO string_table.
    APPEND 'bbbb' TO string_table.
    APPEND 'cccc' TO string_table.

    expected_range_line-sign = 'I'.
    expected_range_line-option = 'NE'.
    expected_range_line-low = 'aaaa'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = 'bbbb'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = 'cccc'.
    APPEND expected_range_line TO expected_range.

    GET REFERENCE OF string_table INTO ref_table.
    ref_range = zcl_data_tool_pre74=>convert_int_table_to_range( i_table = ref_table i_option = 'NE' ).
    ASSIGN ref_range->* TO <fs_actual_range>.

    cl_abap_unit_assert=>assert_equals( act = <fs_actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD num_table_to_range.
    TYPES: ty_num TYPE n LENGTH 3.
    DATA: num_table           TYPE STANDARD TABLE OF ty_num,
          ref_table           TYPE REF TO data,
          ref_range           TYPE REF TO data,
          expected_range      TYPE RANGE OF ty_num,
          expected_range_line LIKE LINE OF expected_range.

    FIELD-SYMBOLS: <fs_actual_range> LIKE expected_range.

    APPEND 'abc' TO num_table.
    APPEND 'cba' TO num_table.
    APPEND 'abc' TO num_table.

    expected_range_line-sign = 'E'.
    expected_range_line-option = 'EQ'.
    expected_range_line-low = 'abc'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = 'cba'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = 'abc'.
    APPEND expected_range_line TO expected_range.

    GET REFERENCE OF num_table INTO ref_table.
    ref_range = zcl_data_tool_pre74=>convert_int_table_to_range( i_table = ref_table i_sign = 'E' ).
    ASSIGN ref_range->* TO <fs_actual_range>.

    cl_abap_unit_assert=>assert_equals( act = <fs_actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD int8_table_to_range.
    DATA: int_table           TYPE STANDARD TABLE OF int8,
          ref_table           TYPE REF TO data,
          ref_range           TYPE REF TO data,
          expected_range      TYPE RANGE OF int8,
          expected_range_line LIKE LINE OF expected_range.

    FIELD-SYMBOLS: <fs_actual_range> LIKE expected_range.

    APPEND 1 TO int_table.
    APPEND 2 TO int_table.
    APPEND 3 TO int_table.

    expected_range_line-sign = 'I'.
    expected_range_line-option = 'EQ'.
    expected_range_line-low = 1.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = 2.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = 3.
    APPEND expected_range_line TO expected_range.

    GET REFERENCE OF int_table INTO ref_table.
    ref_range = zcl_data_tool_pre74=>convert_int_table_to_range( i_table = ref_table ).
    ASSIGN ref_range->* TO <fs_actual_range>.

    cl_abap_unit_assert=>assert_equals( act = <fs_actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD date_table_to_range.
    DATA: date_table          TYPE STANDARD TABLE OF d,
          ref_table           TYPE REF TO data,
          ref_range           TYPE REF TO data,
          expected_range      TYPE RANGE OF d,
          expected_range_line LIKE LINE OF expected_range.

    FIELD-SYMBOLS: <fs_actual_range> LIKE expected_range.

    APPEND '20190101' TO date_table.
    APPEND '20190102' TO date_table.
    APPEND '20190103' TO date_table.

    expected_range_line-sign = 'I'.
    expected_range_line-option = 'EQ'.
    expected_range_line-low = '20190101'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = '20190102'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = '20190103'.
    APPEND expected_range_line TO expected_range.

    GET REFERENCE OF date_table INTO ref_table.
    ref_range = zcl_data_tool_pre74=>convert_int_table_to_range( i_table = ref_table ).
    ASSIGN ref_range->* TO <fs_actual_range>.

    cl_abap_unit_assert=>assert_equals( act = <fs_actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD time_table_to_range.
    DATA: time_table          TYPE STANDARD TABLE OF t,
          ref_table           TYPE REF TO data,
          ref_range           TYPE REF TO data,
          expected_range      TYPE RANGE OF t,
          expected_range_line LIKE LINE OF expected_range.

    FIELD-SYMBOLS: <fs_actual_range> LIKE expected_range.

    APPEND '165129' TO time_table.
    APPEND '165922' TO time_table.
    APPEND '165923' TO time_table.

    expected_range_line-sign = 'I'.
    expected_range_line-option = 'EQ'.
    expected_range_line-low = '165129'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = '165922'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = '165923'.
    APPEND expected_range_line TO expected_range.

    GET REFERENCE OF time_table INTO ref_table.
    ref_range = zcl_data_tool_pre74=>convert_int_table_to_range( i_table = ref_table ).
    ASSIGN ref_range->* TO <fs_actual_range>.

    cl_abap_unit_assert=>assert_equals( act = <fs_actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD ddic_table_to_range.
    DATA: bapibp_curr_table   TYPE STANDARD TABLE OF bapibp_curr,
          ref_table           TYPE REF TO data,
          ref_range           TYPE REF TO data,
          expected_range      TYPE RANGE OF bapibp_curr,
          expected_range_line LIKE LINE OF expected_range.

    FIELD-SYMBOLS: <fs_actual_range> LIKE expected_range.

    APPEND 'AAAA' TO bapibp_curr_table.
    APPEND 'BBBB' TO bapibp_curr_table.
    APPEND 'CCCC' TO bapibp_curr_table.

    expected_range_line-sign = 'I'.
    expected_range_line-option = 'EQ'.
    expected_range_line-low = 'AAAA'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = 'BBBB'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = 'CCCC'.
    APPEND expected_range_line TO expected_range.

    GET REFERENCE OF bapibp_curr_table INTO ref_table.
    ref_range = zcl_data_tool_pre74=>convert_int_table_to_range( i_table = ref_table ).
    ASSIGN ref_range->* TO <fs_actual_range>.

    cl_abap_unit_assert=>assert_equals( act = <fs_actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD struc_table_to_range.
    DATA: struc_table         TYPE STANDARD TABLE OF ty_data,
          struc               TYPE ty_data,
          ref_table           TYPE REF TO data,
          ref_range           TYPE REF TO data,
          expected_range      TYPE RANGE OF nrreturn_dec,
          expected_range_line LIKE LINE OF expected_range.

    FIELD-SYMBOLS: <fs_actual_range> LIKE expected_range.

    struc-number = 1.
    struc-name = 'abc'.
    struc-value = '20'.
    APPEND struc TO struc_table.
    struc-number = 2.
    struc-name = 'cba'.
    struc-value = '40'.
    APPEND struc TO struc_table.
    struc-number = 3.
    struc-name = 'cda'.
    struc-value = '50'.
    APPEND struc TO struc_table.

    expected_range_line-sign = 'E'.
    expected_range_line-option = 'EQ'.
    expected_range_line-low = '20'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = '40'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = '50'.
    APPEND expected_range_line TO expected_range.

    GET REFERENCE OF struc_table INTO ref_table.
    ref_range = zcl_data_tool_pre74=>convert_int_table_to_range( i_table = ref_table i_low_fieldname = 'VALUE' i_sign = 'E' ).
    ASSIGN ref_range->* TO <fs_actual_range>.

    cl_abap_unit_assert=>assert_equals( act = <fs_actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD raise_struc_missing_exc.
    DATA: struc_table TYPE STANDARD TABLE OF ty_data,
          struc       TYPE ty_data,
          ref_table   TYPE REF TO data,
          exception   TYPE REF TO lcx_data_tool_exception.

    struc-number = 1.
    struc-name = 'abc'.
    struc-value = '20'.
    APPEND struc TO struc_table.

    GET REFERENCE OF struc_table INTO ref_table.

    TRY.
        zcl_data_tool_pre74=>convert_int_table_to_range( i_table = ref_table i_sign = 'E' ).
      CATCH lcx_data_tool_exception INTO exception.
    ENDTRY.

    cl_abap_unit_assert=>assert_bound( exception ).
  ENDMETHOD.

  METHOD struc_w_high_table_to_range.
    DATA: struc_table         TYPE STANDARD TABLE OF ty_data,
          struc               TYPE ty_data,
          ref_table           TYPE REF TO data,
          ref_range           TYPE REF TO data,
          expected_range      TYPE RANGE OF nrreturn_dec,
          expected_range_line LIKE LINE OF expected_range.

    FIELD-SYMBOLS: <fs_actual_range> LIKE expected_range.

    struc-number = 1.
    struc-name = 'abc'.
    struc-value = '20'.
    struc-other_value = '200'.
    APPEND struc TO struc_table.
    struc-number = 2.
    struc-name = 'cba'.
    struc-value = '40'.
    struc-other_value = '400'.
    APPEND struc TO struc_table.

    expected_range_line-sign = 'I'.
    expected_range_line-option = 'BT'.
    expected_range_line-low = '20'.
    expected_range_line-high = '200'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-low = '40'.
    expected_range_line-high = '400'.
    APPEND expected_range_line TO expected_range.

    GET REFERENCE OF struc_table INTO ref_table.

    ref_range = zcl_data_tool_pre74=>convert_int_table_to_range(
      i_table = ref_table
      i_low_fieldname = 'VALUE'
      i_high_fieldname = 'OTHER_VALUE'
      i_sign = 'I'
      i_option = 'BT'
    ).

    ASSIGN ref_range->* TO <fs_actual_range>.
    cl_abap_unit_assert=>assert_equals( act = <fs_actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD struc_only_high_table_to_range.
    DATA: struc_table         TYPE STANDARD TABLE OF ty_data,
          struc               TYPE ty_data,
          ref_table           TYPE REF TO data,
          ref_range           TYPE REF TO data,
          expected_range      TYPE RANGE OF nrreturn_dec,
          expected_range_line LIKE LINE OF expected_range.

    FIELD-SYMBOLS: <fs_actual_range> LIKE expected_range.

    struc-number = 1.
    struc-name = 'abc'.
    struc-value = '20'.
    struc-other_value = '200'.
    APPEND struc TO struc_table.
    struc-number = 2.
    struc-name = 'cba'.
    struc-value = '40'.
    struc-other_value = '400'.
    APPEND struc TO struc_table.

    expected_range_line-sign = 'I'.
    expected_range_line-option = 'BT'.
    expected_range_line-high = '200'.
    APPEND expected_range_line TO expected_range.
    expected_range_line-high = '400'.
    APPEND expected_range_line TO expected_range.

    GET REFERENCE OF struc_table INTO ref_table.

    ref_range = zcl_data_tool_pre74=>convert_int_table_to_range(
      i_table = ref_table
      i_high_fieldname = 'OTHER_VALUE'
      i_sign = 'I'
      i_option = 'BT'
    ).

    ASSIGN ref_range->* TO <fs_actual_range>.
    cl_abap_unit_assert=>assert_equals( act = <fs_actual_range> exp = expected_range ).
  ENDMETHOD.

  METHOD fixed_domain_name_to_range.
    DATA: range          TYPE REF TO data,
          expected_range TYPE RANGE OF cccategory.

    FIELD-SYMBOLS: <range> TYPE ty_category_range.

    expected_range = get_category_range(
      i_sign = 'E'
      i_option = 'GT'
    ).

    range = zcl_data_tool_pre74=>convert_domain_to_range(
      i_sign = 'E'
      i_option = 'GT'
      i_domain_name = 'CCCATEGORY'
    ).

    cl_abap_unit_assert=>assert_bound( act = range ).
    ASSIGN range->* TO <range>.

    SORT expected_range BY low.
    SORT <range> BY low.

    cl_abap_unit_assert=>assert_equals( act = <range> exp = expected_range ).
  ENDMETHOD.

  METHOD fixed_domain_ref_to_range.
    DATA: domain         TYPE cccategory,
          domain_ref     TYPE REF TO data,
          range          TYPE REF TO data,
          expected_range TYPE RANGE OF cccategory.

    FIELD-SYMBOLS: <range> TYPE ty_category_range.

    expected_range = get_category_range(
      i_sign = 'E'
      i_option = 'GT'
    ).

    GET  REFERENCE OF domain INTO domain_ref.

    range = zcl_data_tool_pre74=>convert_domain_to_range(
      i_sign = 'E'
      i_option = 'GT'
      i_domain_reference = domain_ref
    ).

    cl_abap_unit_assert=>assert_bound( act = range ).
    ASSIGN range->* TO <range>.

    SORT expected_range BY low.
    SORT <range> BY low.

    cl_abap_unit_assert=>assert_equals( act = <range> exp = expected_range ).
  ENDMETHOD.

  METHOD table_domain_ref_to_range.
    DATA: domain     TYPE mandt,
          domain_ref TYPE REF TO data,
          range      TYPE REF TO data.

    FIELD-SYMBOLS: <range> TYPE STANDARD TABLE.

    GET  REFERENCE OF domain INTO domain_ref.

    range = zcl_data_tool_pre74=>convert_domain_to_range(
      i_sign = 'I'
      i_option = 'EQ'
      i_domain_reference = domain_ref
    ).

    cl_abap_unit_assert=>assert_bound( act = range ).
    ASSIGN range->* TO <range>.


  ENDMETHOD.

  METHOD table_domain_name_to_range.
    DATA: domain_name TYPE domname VALUE 'MANDT',
          range       TYPE REF TO data.

    FIELD-SYMBOLS: <range> TYPE STANDARD TABLE.

    range = zcl_data_tool_pre74=>convert_domain_to_range(
      i_sign = 'I'
      i_option = 'EQ'
      i_domain_name = domain_name
    ).

    cl_abap_unit_assert=>assert_bound( act = range ).
    ASSIGN range->* TO <range>.
  ENDMETHOD.

  METHOD get_category_range.
    DATA: range_line LIKE LINE OF r_range.

    range_line-sign = i_sign.
    range_line-option = i_option.
    range_line-low = 'P'.
    APPEND range_line TO r_range.
    range_line-low = 'T'.
    APPEND range_line TO r_range.
    range_line-low = 'C'.
    APPEND range_line TO r_range.
    range_line-low = 'D'.
    APPEND range_line TO r_range.
    range_line-low = 'E'.
    APPEND range_line TO r_range.
    range_line-low = 'S'.
    APPEND range_line TO r_range.
  ENDMETHOD.

  METHOD get_mandt_range.
    DATA: range_line LIKE LINE OF r_range.

    range_line-sign = i_sign.
    range_line-option = i_option.
    range_line-low = '000'.
    APPEND range_line TO r_range.
    range_line-low = '001'.
    APPEND range_line TO r_range.
  ENDMETHOD.

ENDCLASS.
