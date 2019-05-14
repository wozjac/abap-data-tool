TYPES: BEGIN OF ty_type_details,
         length    TYPE i,
         decimals  TYPE i,
         line_type TYPE c LENGTH 1,
         kind      TYPE abap_typekind,
       END OF ty_type_details,

       ty_domain_values TYPE STANDARD TABLE OF dd07v.

CLASS lcx_data_tool_exception DEFINITION INHERITING FROM cx_no_check CREATE PUBLIC.

ENDCLASS.
