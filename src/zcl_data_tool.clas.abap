CLASS zcl_data_tool DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS:
      internal_table_to_range IMPORTING i_table        TYPE REF TO data
                                        i_fieldname    TYPE string OPTIONAL
                                        i_sign         TYPE char1 DEFAULT 'I'
                                        i_option       TYPE char2 DEFAULT 'EQ'
                              RETURNING VALUE(r_range) TYPE REF TO data.
  PRIVATE SECTION.
    CLASS-METHODS:
      create_low_high_components IMPORTING i_range_value_details TYPE ty_data_details
                                 RETURNING VALUE(r_components)   TYPE abap_component_tab,
      create_range_components IMPORTING i_range_value_details TYPE ty_data_details
                              RETURNING VALUE(r_components)   TYPE abap_component_tab,
      get_value_type_details IMPORTING i_table               TYPE REF TO data
                                       i_fieldname           TYPE string OPTIONAL
                             RETURNING VALUE(r_data_details) TYPE ty_data_details.
ENDCLASS.

CLASS zcl_data_tool IMPLEMENTATION.
  METHOD internal_table_to_range.
    DATA:
      structure_description TYPE REF TO cl_abap_structdescr,
      table_description     TYPE REF TO cl_abap_tabledescr,
      data_description      TYPE REF TO cl_abap_datadescr,
      lo_elemdescr          TYPE REF TO cl_abap_elemdescr,
      components            TYPE abap_component_tab,
      lt_itab_comp          TYPE abap_compdescr_tab,
      lv_linetype,
      lv_kind               TYPE abap_typekind,
      ls_component          TYPE LINE OF abap_component_tab,
      range_line            TYPE REF TO data,
      range_table           TYPE REF TO data,
      value_data_details    TYPE ty_data_details,
      lv_length             TYPE i,
      lv_dec                TYPE i.

    FIELD-SYMBOLS: <range_line>   TYPE any,
                   <sign>         TYPE any,
                   <option>       TYPE any,
                   <low>          TYPE any,
                   <itab_comp>    LIKE LINE OF lt_itab_comp,
                   <source_table> TYPE ANY TABLE,
                   <line>         TYPE any,
                   <value>        TYPE any,
                   <range_table>  TYPE STANDARD TABLE.

* components of structure -> lt_components
*    MOVE 'SIGN' TO ls_component-name.
*    ls_component-type = cl_abap_elemdescr=>get_c( p_length = 1 ).
*    INSERT ls_component INTO TABLE lt_components.
*
*    MOVE 'OPTION' TO ls_component-name.
*    ls_component-type = cl_abap_elemdescr=>get_c( p_length = 2 ).
*    INSERT ls_component INTO TABLE lt_components.

* Describe fields from internal table
*    lo_tabledescr ?= cl_abap_tabledescr=>describe_by_data_ref( i_table ).
*    lo_datadescr = lo_tabledescr->get_table_line_type( ).

*    "line type structure
*    IF cl_lcr_util=>instanceof(
*      object = lo_datadescr
*      class = 'CL_ABAP_STRUCTDESCR' ) = abap_true.
*
*      lo_structdescr ?= lo_datadescr.
*      lt_itab_comp = lo_structdescr->components.
*      lv_linetype = 'S'.
*
*      READ TABLE lt_itab_comp ASSIGNING <fs_itab_comp>
*        WITH KEY name = i_fieldname.                        "#EC WARNOK
*
*      IF sy-subrc <> 0.
*        RAISE EXCEPTION TYPE lcx_data_tool_exception.
*      ENDIF.
*
*      lv_length = <fs_itab_comp>-length / 2.
*      lv_kind = <fs_itab_comp>-type_kind.
*
*    ENDIF.
*
*    "line type element
*    IF cl_lcr_util=>instanceof(
*      object = lo_datadescr
*      class = 'CL_ABAP_ELEMDESCR' ) = abap_true.
*
*      lo_elemdescr ?= lo_datadescr.
*      lv_kind = lo_elemdescr->type_kind.
*      lv_length = lo_elemdescr->length / 2.
*      lv_linetype = 'E'.
*
*    ENDIF.

    value_data_details = get_value_type_details(
        i_table = i_table
        i_fieldname = i_fieldname
    ).

    components = create_range_components( i_range_value_details = value_data_details ).
*    CASE lv_kind.
*      WHEN 'C'.
*        MOVE 'LOW' TO ls_component-name.
*        ls_component-type = cl_abap_elemdescr=>get_c( p_length = lv_length ).
*        INSERT ls_component INTO TABLE lt_components.
*
*        MOVE 'HIGH' TO ls_component-name.
*        ls_component-type = cl_abap_elemdescr=>get_c( p_length = lv_length ).
*        INSERT ls_component INTO TABLE lt_components.
*
*      WHEN 'N'.
*        MOVE 'LOW' TO ls_component-name.
*        ls_component-type = cl_abap_elemdescr=>get_n( p_length = lv_length ).
*        INSERT ls_component INTO TABLE lt_components.
*
*        MOVE 'HIGH' TO ls_component-name.
*        ls_component-type = cl_abap_elemdescr=>get_n( p_length = lv_length ).
*        INSERT ls_component INTO TABLE lt_components.
*
*      WHEN 'D'.
*        lv_dec = <fs_itab_comp>-decimals.
*        MOVE 'LOW' TO ls_component-name.
*
*        ls_component-type = cl_abap_elemdescr=>get_p(
*          p_length = lv_length
*          p_decimals = lv_dec
*        ).
*
*        INSERT ls_component INTO TABLE lt_components.
*
*        MOVE 'HIGH' TO ls_component-name.
*
*        ls_component-type = cl_abap_elemdescr=>get_p(
*          p_length = lv_length
*          p_decimals = lv_dec
*        ).
*
*        INSERT ls_component INTO TABLE lt_components.
*
*      WHEN 'I'.
*        MOVE 'LOW' TO ls_component-name.
*        ls_component-type = cl_abap_elemdescr=>get_i( ).
*        INSERT ls_component INTO TABLE lt_components.
*
*        MOVE 'HIGH' TO ls_component-name.
*        ls_component-type = cl_abap_elemdescr=>get_i( ).
*        INSERT ls_component INTO TABLE lt_components.
*
*      WHEN 'F'.
*        MOVE 'LOW' TO ls_component-name.
*        ls_component-type = cl_abap_elemdescr=>get_f( ).
*        INSERT ls_component INTO TABLE lt_components.
*
*        MOVE 'HIGH' TO ls_component-name.
*        ls_component-type = cl_abap_elemdescr=>get_f( ).
*        INSERT ls_component INTO TABLE lt_components.
*
*      WHEN 'a'.
*        MOVE 'LOW' TO ls_component-name.
*        ls_component-type = cl_abap_elemdescr=>get_decfloat16( ).
*        INSERT ls_component INTO TABLE lt_components.
*
*        MOVE 'HIGH' TO ls_component-name.
*        ls_component-type = cl_abap_elemdescr=>get_decfloat16( ).
*        INSERT ls_component INTO TABLE lt_components.
*
*      WHEN OTHERS.
*        RAISE EXCEPTION TYPE lcx_data_tool_exception.
*    ENDCASE.

    structure_description = cl_abap_structdescr=>create( components ).
    CREATE DATA range_line TYPE HANDLE structure_description.
    ASSIGN range_line->* TO <range_line>.
    data_description = structure_description.
    table_description = cl_abap_tabledescr=>create( data_description ).
    CREATE DATA range_table TYPE HANDLE table_description.
    ASSIGN range_table->* TO <range_table>.
    ASSIGN i_table->* TO <source_table>.

    LOOP AT <source_table> ASSIGNING <line>.
      CASE value_data_details-line_type.
        WHEN 'S'.
          ASSIGN COMPONENT i_fieldname OF STRUCTURE <line> TO <value>.

        WHEN 'E'.
          ASSIGN <line> TO <value>.
      ENDCASE.

      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <range_line> TO <sign>.
      <sign> = i_sign.
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <range_line> TO <option>.
      <option> = i_option.
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <range_line> TO <low>.
      <low> = <value>.
      APPEND <range_line> TO <range_table>.
    ENDLOOP.

    GET REFERENCE OF <range_table> INTO r_range.
  ENDMETHOD.

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

  METHOD get_value_type_details.
    DATA: is_given_type       TYPE c LENGTH 1,
          table_description   TYPE REF TO cl_abap_tabledescr,
          data_description    TYPE REF TO cl_abap_datadescr,
          struct_description  TYPE REF TO cl_abap_structdescr,
          element_description TYPE REF TO cl_abap_elemdescr,
          itab_components     TYPE abap_compdescr_tab.

    FIELD-SYMBOLS: <fs_itab_component> LIKE LINE OF itab_components.

    table_description ?= cl_abap_tabledescr=>describe_by_data_ref( i_table ).
    data_description = table_description->get_table_line_type( ).

    is_given_type = cl_lcr_util=>instanceof(
      object = data_description
      class = 'CL_ABAP_STRUCTDESCR'
    ).

    "line type structure
    IF is_given_type = abap_true.
      struct_description ?= data_description.
      itab_components = struct_description->components.

      READ TABLE itab_components ASSIGNING <fs_itab_component>
        WITH KEY name = i_fieldname.                        "#EC WARNOK

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_data_tool_exception.
      ENDIF.

      r_data_details-length = <fs_itab_component>-length.
      r_data_details-kind = <fs_itab_component>-type_kind.
      r_data_details-length = <fs_itab_component>-length.
      r_data_details-decimals = <fs_itab_component>-decimals.
      r_data_details-line_type = 'S'.
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
      r_data_details-line_type = 'E'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
