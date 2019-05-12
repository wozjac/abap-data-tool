TYPES: BEGIN OF ty_type_details,
         length    TYPE i,
         decimals  TYPE i,
         line_type TYPE c LENGTH 1,
         kind      TYPE abap_typekind,
       END OF ty_type_details.

CLASS lcx_data_tool_exception DEFINITION INHERITING FROM cx_no_check CREATE PUBLIC.

ENDCLASS.
