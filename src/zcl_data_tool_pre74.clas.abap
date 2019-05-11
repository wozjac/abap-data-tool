"! <p class="shorttext synchronized" lang="en">Data utility tools, compatible with NW versions &lt; 7.4.</p>
"! <p>For the same functionality with more expression-style code (NW 7.4 and later), go to class ZCL_DATA_TOOL.</p>
CLASS zcl_data_tool_pre74 DEFINITION PUBLIC FINAL CREATE PUBLIC.
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
      "! @parameter i_low_fieldname | <p class="shorttext synchronized" lang="en">Field name in the structure for LOW range field values</p>
      "! <p>Relevant if the line type of the internal table; the name of the structure field used for getting
      "! values for LOW range component</p>
      "! @parameter i_high_fieldname | <p class="shorttext synchronized" lang="en">Field name in the structure for HIGH range field values</p>
      "! <p>Relevant if the line type of the internal table; the name of the structure field used for getting
      "! values for HIGH range component</p>
      "! @parameter i_sign | <p class="shorttext synchronized" lang="en">SIGN field value in the created range</p>
      "! @parameter i_option | <p class="shorttext synchronized" lang="en">OPTION field value in the created range</p>
      "! @parameter r_range | <p class="shorttext synchronized" lang="en">A data reference to the created range</p>
      internal_table_to_range IMPORTING i_table          TYPE REF TO data
                                        i_low_fieldname  TYPE string OPTIONAL
                                        i_high_fieldname TYPE string OPTIONAL
                                        i_sign           TYPE char1 DEFAULT 'I'
                                        i_option         TYPE char2 DEFAULT 'EQ'
                              RETURNING VALUE(r_range)   TYPE REF TO data.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_line_type,
        structure TYPE c LENGTH 1 VALUE 'S',
        element   TYPE c LENGTH 1 VALUE 'E',
      END OF c_line_type.

    CLASS-METHODS:
      create_low_high_components IMPORTING i_range_value_details TYPE ty_data_details
                                 RETURNING VALUE(r_components)   TYPE abap_component_tab,
      create_range_components IMPORTING i_range_value_details TYPE ty_data_details
                              RETURNING VALUE(r_components)   TYPE abap_component_tab,
      get_value_type_details IMPORTING i_table               TYPE REF TO data
                                       i_low_fieldname       TYPE string OPTIONAL
                                       i_high_fieldname      TYPE string OPTIONAL
                             RETURNING VALUE(r_data_details) TYPE ty_data_details,
      create_range_table IMPORTING i_structure_description TYPE REF TO cl_abap_structdescr
                         RETURNING VALUE(r_range_table)    TYPE REF TO data.
ENDCLASS.

CLASS zcl_data_tool_pre74 IMPLEMENTATION.
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
            p_decimals = i_range_value_details-decimals
          ).
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
    DATA: component           LIKE LINE OF r_components,
          low_high_components LIKE r_components.

    component-name = 'SIGN'.
    component-type = cl_abap_elemdescr=>get_c( p_length = 1 ).
    INSERT component INTO TABLE r_components.

    component-name = 'OPTION'.
    component-type = cl_abap_elemdescr=>get_c( p_length = 2 ).
    INSERT component INTO TABLE r_components.

    low_high_components = create_low_high_components( i_range_value_details = i_range_value_details ).
    INSERT LINES OF low_high_components INTO TABLE r_components.
  ENDMETHOD.


  METHOD create_range_table.
    DATA: table_description TYPE REF TO cl_abap_tabledescr,
          data_description  TYPE REF TO cl_abap_datadescr.

    data_description = i_structure_description.
    table_description = cl_abap_tabledescr=>create( data_description ).
    CREATE DATA r_range_table TYPE HANDLE table_description.
  ENDMETHOD.

  METHOD get_value_type_details.
    DATA: is_given_type       TYPE c LENGTH 1,
          table_description   TYPE REF TO cl_abap_tabledescr,
          data_description    TYPE REF TO cl_abap_datadescr,
          struct_description  TYPE REF TO cl_abap_structdescr,
          element_description TYPE REF TO cl_abap_elemdescr,
          itab_components     TYPE abap_compdescr_tab,
          field_name          TYPE string.

    FIELD-SYMBOLS: <fs_itab_component> LIKE LINE OF itab_components.

    table_description ?= cl_abap_tabledescr=>describe_by_data_ref( i_table ).
    data_description = table_description->get_table_line_type( ).

    is_given_type = cl_lcr_util=>instanceof(
      object = data_description
      class = 'CL_ABAP_STRUCTDESCR'
    ).

    "line type structure
    IF is_given_type = abap_true.
      IF i_low_fieldname IS INITIAL
        AND i_high_fieldname IS INITIAL.

        RAISE EXCEPTION TYPE lcx_data_tool_exception.
      ENDIF.

      struct_description ?= data_description.
      itab_components = struct_description->components.

      IF i_low_fieldname IS NOT INITIAL.
        field_name = i_low_fieldname.
      ELSE.
        field_name = i_high_fieldname.
      ENDIF.

      READ TABLE itab_components ASSIGNING <fs_itab_component>
        WITH KEY name = field_name.                         "#EC WARNOK

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_data_tool_exception.
      ENDIF.

      r_data_details-length = <fs_itab_component>-length.
      r_data_details-kind = <fs_itab_component>-type_kind.
      r_data_details-length = <fs_itab_component>-length.
      r_data_details-decimals = <fs_itab_component>-decimals.
      r_data_details-line_type = c_line_type-structure.
    ENDIF.

    is_given_type = cl_lcr_util=>instanceof(
      object = data_description
      class = 'CL_ABAP_ELEMDESCR'
    ).

    IF is_given_type = abap_true.
      element_description ?= data_description.
      r_data_details-kind = element_description->type_kind.
      r_data_details-length = element_description->length.
      r_data_details-decimals = element_description->decimals.
      r_data_details-line_type = c_line_type-element.
    ENDIF.
  ENDMETHOD.

  METHOD internal_table_to_range.
    DATA:
      structure_description TYPE REF TO cl_abap_structdescr,
      components            TYPE abap_component_tab,
      range_line            TYPE REF TO data,
      value_data_details    TYPE ty_data_details.

    FIELD-SYMBOLS: <range_line>   TYPE any,
                   <sign>         TYPE any,
                   <option>       TYPE any,
                   <low>          TYPE any,
                   <high>         TYPE any,
                   <source_table> TYPE ANY TABLE,
                   <source_line>  TYPE any,
                   <value>        TYPE any,
                   <value_high>   TYPE any,
                   <range_table>  TYPE STANDARD TABLE.

    value_data_details = get_value_type_details(
        i_table = i_table
        i_low_fieldname = i_low_fieldname
        i_high_fieldname = i_high_fieldname
    ).

    components = create_range_components( i_range_value_details = value_data_details ).
    structure_description = cl_abap_structdescr=>create( components ).
    r_range = create_range_table( i_structure_description = structure_description ).

    ASSIGN r_range->* TO <range_table>.
    ASSIGN i_table->* TO <source_table>.
    CREATE DATA range_line TYPE HANDLE structure_description.
    ASSIGN range_line->* TO <range_line>.

    LOOP AT <source_table> ASSIGNING <source_line>.
      CASE value_data_details-line_type.
        WHEN c_line_type-structure.
          IF i_low_fieldname IS NOT INITIAL.
            ASSIGN COMPONENT i_low_fieldname OF STRUCTURE <source_line> TO <value>.
          ENDIF.

          IF i_high_fieldname IS NOT INITIAL.
            ASSIGN COMPONENT i_high_fieldname OF STRUCTURE <source_line> TO <value_high>.
          ENDIF.

        WHEN c_line_type-element.
          ASSIGN <source_line> TO <value>.
      ENDCASE.

      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <range_line> TO <sign>.
      <sign> = i_sign.
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <range_line> TO <option>.
      <option> = i_option.

      IF <value> IS ASSIGNED.
        ASSIGN COMPONENT 'LOW' OF STRUCTURE <range_line> TO <low>.
        <low> = <value>.
      ENDIF.

      IF <value_high> IS ASSIGNED.
        ASSIGN COMPONENT 'HIGH' OF STRUCTURE <range_line> TO <high>.
        <high> = <value_high>.
      ENDIF.

      APPEND <range_line> TO <range_table>.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
