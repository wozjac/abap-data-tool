"! <p class="shorttext synchronized" lang="en">Data utility tools for NW version 7.4 and above.</p>
"! <p>For the same functionality for NW version lower than 7.4, go to class ZCL_DATA_TOOL_PRE74.</p>
"! <p>All errors are thrown as pure <em>lcx_data_tool_exception</em> exception (superclass CX_NO_CHECK), adjust this as needed</p>
CLASS zcl_data_tool DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Converts an internal table to a range.</p>
      "! <ul>Features:
      "! <li>supports non- and structured line type</li>
      "! <li>works for elementary types (or DDIC types based on): p, c, n, d, t, f, i, int8, string, decfloat16, decfloat34</li>
      "! </ul>
      "! <p>Usage:</p>
      "! <p>The internal table has to be provided as data reference and the final range is returned as reference too.
      "! If the table is non-structured, <em>i_low_fieldname</em> and <em>i_high_fieldname</em> are not relevant.
      "! If the table is structured, at least one field name of the structure has to be provided.</p>
      "! <p>Examples: see the local test class <em>lcl_test</em>.</p>
      "!
      "! @parameter i_table | <p class="shorttext synchronized" lang="en">A data reference to the internal table</p>
      "! @parameter i_low_fieldname | <p class="shorttext synchronized" lang="en">Field name in the structure for
      "! LOW range field values</p>
      "! <p>Relevant if the line type of the internal table; the name of the structure field used for getting
      "! values for LOW range component</p>
      "! @parameter i_high_fieldname | <p class="shorttext synchronized" lang="en">Field name in the structure for
      "! HIGH range field values</p>
      "! <p>Relevant if the line type of the internal table; the name of the structure field used for getting
      "! values for HIGH range component</p>
      "! @parameter i_sign | <p class="shorttext synchronized" lang="en">SIGN field value in the created range (default I)</p>
      "! @parameter i_option | <p class="shorttext synchronized" lang="en">OPTION field value in the created range (default EQ)</p>
      "! @parameter r_range | <p class="shorttext synchronized" lang="en">A data reference to the created range</p>
      convert_int_table_to_range IMPORTING i_table          TYPE REF TO data
                                           i_low_fieldname  TYPE string OPTIONAL
                                           i_high_fieldname TYPE string OPTIONAL
                                           i_sign           TYPE char1 DEFAULT 'I'
                                           i_option         TYPE char2 DEFAULT 'EQ'
                                 RETURNING VALUE(r_range)   TYPE REF TO data,

      "! <p class="shorttext synchronized" lang="en">Converts a domain to a range</p>
      "! <ul>Features:
      "! <li>supports domains with fixed values and value tables</li>
      "! <li>supported types: see documentation <em>convert_int_table_to_range</em></li>
      "! </ul>
      "! <p>Usage: pass a domain name or a reference to a data object with domain. The result range is
      "! returned as a data reference.</p>
      "! <p>Examples: see the local test class <em>lcl_test</em>.</p>
      "! @parameter i_sign | <p class="shorttext synchronized" lang="en">SIGN field value in the created range (default I)</p>
      "! @parameter i_option | <p class="shorttext synchronized" lang="en">OPTION field value in the created range (default EQ)</p>
      "! @parameter i_domain_name | <p class="shorttext synchronized" lang="en">Domain name</p>
      "! @parameter i_domain_reference | <p class="shorttext synchronized" lang="en">A reference to a data object with domain type</p>
      "! @parameter r_range | <p class="shorttext synchronized" lang="en">A data reference to the created range</p>
      convert_domain_to_range IMPORTING i_sign             TYPE char1 DEFAULT 'I'
                                        i_option           TYPE char2 DEFAULT 'EQ'
                                        i_domain_name      TYPE domname OPTIONAL
                                        i_domain_reference TYPE REF TO data OPTIONAL
                              RETURNING VALUE(r_range)     TYPE REF TO data.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_line_type,
        structure TYPE c LENGTH 1 VALUE 'S',
        element   TYPE c LENGTH 1 VALUE 'E',
      END OF c_line_type.

    CLASS-METHODS:
      create_low_high_components IMPORTING i_range_value_details TYPE ty_type_details
                                 RETURNING VALUE(r_components)   TYPE abap_component_tab,

      create_range_components IMPORTING i_range_value_details TYPE ty_type_details
                              RETURNING VALUE(r_components)   TYPE abap_component_tab,

      create_range_table IMPORTING i_structure_description TYPE REF TO cl_abap_structdescr
                         RETURNING VALUE(r_range_table)    TYPE REF TO data,

      get_type_details_from_element IMPORTING i_domain_name         TYPE domname
                                    RETURNING VALUE(r_type_details) TYPE ty_type_details,

      get_type_details_from_table IMPORTING i_table               TYPE REF TO data
                                            i_low_fieldname       TYPE string OPTIONAL
                                            i_high_fieldname      TYPE string OPTIONAL
                                  RETURNING VALUE(r_type_details) TYPE ty_type_details,

      get_value_table_field IMPORTING i_table_name       TYPE char30
                                      i_domain_name      TYPE domname
                            RETURNING VALUE(r_fieldname) TYPE string,

      get_value_table_values IMPORTING i_table_name  TYPE char30
                                       i_domain_name TYPE domname
                             EXPORTING e_values      TYPE ty_domain_values.
ENDCLASS.

CLASS zcl_data_tool IMPLEMENTATION.
  METHOD create_low_high_components.
    DATA: component LIKE LINE OF r_components,
          names     TYPE STANDARD TABLE OF char10,
          name      LIKE LINE OF names.

    APPEND 'LOW' TO names.
    APPEND 'HIGH' TO names.

    LOOP AT names INTO name.
      component-name = name.

      CASE i_range_value_details-kind.
        WHEN 'C'.
          component-type = cl_abap_elemdescr=>get_c( p_length = i_range_value_details-length / 2 ).
        WHEN 'N'.
          component-type = cl_abap_elemdescr=>get_n( p_length = i_range_value_details-length / 2 ).
        WHEN 'P'.
          component-type = cl_abap_elemdescr=>get_p(
            p_length = i_range_value_details-length
            p_decimals = i_range_value_details-decimals ).
        WHEN 'D'.
          component-type = cl_abap_elemdescr=>get_d( ).
        WHEN 'T'.
          component-type = cl_abap_elemdescr=>get_t( ).
        WHEN 'I'.
          component-type = cl_abap_elemdescr=>get_i( ).
        WHEN '8'.
          component-type = cl_abap_elemdescr=>get_int8( ).
        WHEN 'F'.
          component-type = cl_abap_elemdescr=>get_f( ).
        WHEN 'a'.
          component-type = cl_abap_elemdescr=>get_decfloat16( ).
        WHEN 'e'.
          component-type = cl_abap_elemdescr=>get_decfloat34( ).
        WHEN 'g'.
          component-type = cl_abap_elemdescr=>get_string( ).
        WHEN OTHERS.
          RAISE EXCEPTION TYPE lcx_data_tool_exception.
      ENDCASE.

      INSERT component INTO TABLE r_components.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_range_components.
    r_components = VALUE #(
      (
        name = 'SIGN'
        type = cl_abap_elemdescr=>get_c( p_length = 1 )
      )
      (
        name = 'OPTION'
        type = cl_abap_elemdescr=>get_c( p_length = 2 )
      )
    ).

    DATA(low_high_components) = create_low_high_components( i_range_value_details = i_range_value_details ).
    r_components = VALUE #( BASE r_components ( LINES OF low_high_components ) ).
  ENDMETHOD.

  METHOD create_range_table.
    DATA: table_description TYPE REF TO cl_abap_tabledescr,
          data_description  TYPE REF TO cl_abap_datadescr.

    data_description = i_structure_description.
    table_description = cl_abap_tabledescr=>create( data_description ).
    CREATE DATA r_range_table TYPE HANDLE table_description.
  ENDMETHOD.

  METHOD convert_domain_to_range.
    DATA: domain_values           TYPE STANDARD TABLE OF dd07v,
          domain_type_description TYPE dd01v,
          domain_name             LIKE i_domain_name,
          range_line              TYPE REF TO data.

    FIELD-SYMBOLS: <domain_value> LIKE LINE OF domain_values,
                   <range_table>  TYPE STANDARD TABLE.

    IF i_domain_name IS INITIAL AND i_domain_reference IS INITIAL.
      RAISE EXCEPTION TYPE lcx_data_tool_exception.
    ENDIF.

    IF i_domain_reference IS NOT INITIAL.
      DATA(element_description) = CAST cl_abap_elemdescr( cl_abap_datadescr=>describe_by_data_ref( p_data_ref = i_domain_reference ) ).
      domain_name = element_description->get_ddic_field( )-domname.
    ELSE.
      domain_name = i_domain_name.
    ENDIF.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = domain_name
        langu         = sy-langu
      IMPORTING
        dd01v_wa      = domain_type_description
      TABLES
        dd07v_tab     = domain_values
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_data_tool_exception.
    ENDIF.

    IF domain_type_description-entitytab IS NOT INITIAL.
      get_value_table_values(
        EXPORTING
          i_table_name = domain_type_description-entitytab
          i_domain_name = domain_name
        IMPORTING e_values = domain_values ).
    ENDIF.

    DATA(type_details) = get_type_details_from_element( i_domain_name = domain_name ).
    DATA(structure_description) = cl_abap_structdescr=>create( create_range_components( i_range_value_details = type_details ) ).
    DATA(table_description) = cl_abap_tabledescr=>create( structure_description ).
    CREATE DATA r_range TYPE HANDLE table_description.
    CREATE DATA range_line TYPE HANDLE structure_description.
    ASSIGN r_range->* TO <range_table>.
    ASSIGN range_line->* TO FIELD-SYMBOL(<range_line>).

    LOOP AT domain_values ASSIGNING <domain_value>.
      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <range_line> TO FIELD-SYMBOL(<sign>).
      <sign> = i_sign.
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <range_line> TO FIELD-SYMBOL(<option>).
      <option> = i_option.

      IF <domain_value>-domvalue_l IS NOT INITIAL.
        ASSIGN COMPONENT 'LOW' OF STRUCTURE <range_line> TO FIELD-SYMBOL(<low>).
        <low> = <domain_value>-domvalue_l.
      ENDIF.

      IF <domain_value>-domvalue_h IS NOT INITIAL.
        ASSIGN COMPONENT 'HIGH' OF STRUCTURE <range_line> TO FIELD-SYMBOL(<high>).
        <high> = <domain_value>-domvalue_h.
      ENDIF.

      APPEND <range_line> TO <range_table>.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_type_details_from_element.
    DATA(element_description) = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_name( EXPORTING p_name = i_domain_name ) ).
    r_type_details-kind = element_description->type_kind.
    r_type_details-length = element_description->length.
    r_type_details-decimals = element_description->decimals.
    r_type_details-line_type = c_line_type-element.
  ENDMETHOD.

  METHOD get_type_details_from_table.
    DATA: field_name          TYPE string.

    DATA(table_description) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( i_table ) ).
    DATA(data_description) = table_description->get_table_line_type( ).

    "line type structure
    IF data_description IS INSTANCE OF cl_abap_structdescr.
      IF i_low_fieldname IS INITIAL AND i_high_fieldname IS INITIAL.
        RAISE EXCEPTION TYPE lcx_data_tool_exception.
      ENDIF.

      DATA(struct_description) = CAST cl_abap_structdescr( data_description ).

      IF i_low_fieldname IS NOT INITIAL.
        field_name = i_low_fieldname.
      ELSE.
        field_name = i_high_fieldname.
      ENDIF.

      TRY.
          DATA(itab_component) = struct_description->components[ name = field_name ].
        CATCH cx_sy_itab_line_not_found.
          RAISE EXCEPTION TYPE lcx_data_tool_exception.
      ENDTRY.

      r_type_details-length = itab_component-length.
      r_type_details-kind = itab_component-type_kind.
      r_type_details-length = itab_component-length.
      r_type_details-decimals = itab_component-decimals.
      r_type_details-line_type = c_line_type-structure.
    ENDIF.

    IF data_description IS INSTANCE OF cl_abap_elemdescr.
      DATA(element_description) = CAST cl_abap_elemdescr( data_description ).
      r_type_details-kind = element_description->type_kind.
      r_type_details-length = element_description->length.
      r_type_details-decimals = element_description->decimals.
      r_type_details-line_type = c_line_type-element.
    ENDIF.
  ENDMETHOD.

  METHOD convert_int_table_to_range.
    DATA: range_line            TYPE REF TO data.
    FIELD-SYMBOLS: <source_table> TYPE ANY TABLE,
                   <range_table>  TYPE STANDARD TABLE.

    DATA(value_data_details) = get_type_details_from_table(
        i_table = i_table
        i_low_fieldname = i_low_fieldname
        i_high_fieldname = i_high_fieldname ).

    DATA(structure_description) = cl_abap_structdescr=>create(
      create_range_components( i_range_value_details = value_data_details ) ).

    DATA(table_description) = cl_abap_tabledescr=>create( structure_description ).
    CREATE DATA r_range TYPE HANDLE table_description.
    CREATE DATA range_line TYPE HANDLE structure_description.
    ASSIGN r_range->* TO <range_table>.
    ASSIGN i_table->* TO <source_table>.
    ASSIGN range_line->* TO FIELD-SYMBOL(<range_line>).

    LOOP AT <source_table> ASSIGNING FIELD-SYMBOL(<source_line>).
      CASE value_data_details-line_type.
        WHEN c_line_type-structure.
          IF i_low_fieldname IS NOT INITIAL.
            ASSIGN COMPONENT i_low_fieldname OF STRUCTURE <source_line> TO FIELD-SYMBOL(<value>).
          ENDIF.

          IF i_high_fieldname IS NOT INITIAL.
            ASSIGN COMPONENT i_high_fieldname OF STRUCTURE <source_line> TO FIELD-SYMBOL(<value_high>).
          ENDIF.

        WHEN c_line_type-element.
          ASSIGN <source_line> TO <value>.
      ENDCASE.

      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <range_line> TO FIELD-SYMBOL(<sign>).
      <sign> = i_sign.
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <range_line> TO FIELD-SYMBOL(<option>).
      <option> = i_option.

      IF <value> IS ASSIGNED.
        ASSIGN COMPONENT 'LOW' OF STRUCTURE <range_line> TO FIELD-SYMBOL(<low>).
        <low> = <value>.
      ENDIF.

      IF <value_high> IS ASSIGNED.
        ASSIGN COMPONENT 'HIGH' OF STRUCTURE <range_line> TO FIELD-SYMBOL(<high>).
        <high> = <value_high>.
      ENDIF.

      APPEND <range_line> TO <range_table>.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_value_table_field.
    DATA(struct_description) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( p_name = i_table_name ) ).
    DATA(fields) = struct_description->get_ddic_field_list( ).

    TRY.
        r_fieldname = fields[ domname = i_domain_name ]-fieldname.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE lcx_data_tool_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD get_value_table_values.
    DATA: value_low_tab TYPE STANDARD TABLE OF domvalue_l.

    DATA(field_name) = get_value_table_field(
        i_table_name = i_table_name
        i_domain_name = i_domain_name
      ).

    SELECT (field_name) FROM (i_table_name) INTO TABLE value_low_tab.
    e_values = VALUE #( FOR line IN value_low_tab ( domvalue_l = line ) ).
  ENDMETHOD.
ENDCLASS.
