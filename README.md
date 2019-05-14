# ABAP Data Tools
Various ABAP utilities.

## Classes
### ZCL_DATA_TOOL and ZCL_DATA_TOOL_PRE74
Both classes deliver the same functionality, but the former is for NW system version > 7.4 and has more expression-style usage, while the latter is dedicated for NW system version < 7.4. 
#### Methods:
##### convert_int_table_to_range
Converts an internal table to a range.  
Features:
- supports non- and structured line type
- works for elementary types (or DDIC types based on): p, c, n, d, t, f, i, int8, string, decfloat16, decfloat34</li>  

Usage:  
The internal table has to be provided as data reference and the final range is returned as reference too.
If the table is non-structured,*i_low_fieldname* and *i_high_fieldname* are not relevant. If the table is structured, at least one field name of the structure has to be provided.  
Examples (ZCL_DATA_TOOL_PRE74): 
* non-structured table
```ABAP
DATA: int_table   TYPE STANDARD TABLE OF i,
      ref_table   TYPE REF TO data,
      ref_range   TYPE REF TO data,
      final_range TYPE RANGE OF i.

FIELD-SYMBOLS: <range> LIKE final_range.

APPEND 1 TO int_table.
APPEND 2 TO int_table.
APPEND 3 TO int_table.

GET REFERENCE OF int_table INTO ref_table.
"default SIGN = I, OPTION = EQ
ref_range = zcl_data_tool_pre74=>internal_table_to_range( i_table = ref_table ). 
ASSIGN ref_range->* to <range>.
```  
* structured table
```ABAP
TYPES: BEGIN OF ty_data,
         name  TYPE string,
         value TYPE i,
       END OF ty_data.

DATA: struc_table TYPE STANDARD TABLE OF ty_data,
      struc       TYPE ty_data,
      ref_table   TYPE REF TO data,
      ref_range   TYPE REF TO data,
      final_range TYPE RANGE OF ty_data.

FIELD-SYMBOLS: <range> LIKE final_range.

struc-name = 'abc'.
struc-value = '20'.
APPEND struc TO struc_table.
struc-name = 'cba'.
struc-value = '40'.
APPEND struc TO struc_table.

GET REFERENCE OF struc_table INTO ref_table.

ref_range = zcl_data_tool_pre74=>internal_table_to_range( 
  i_table = ref_table 
  i_low_fieldname = 'VALUE' 
  i_sign = 'E' 
).

ASSIGN ref_range->* TO <range>.
```  
##### convert_domain_to_range
Examples (ZCL_DATA_TOOL_PRE74):
* passing a data object with a domain type
```ABAP
DATA: bapiclient      TYPE bapimandt,
      range_reference TYPE REF TO data,
      clients         TYPE RANGE OF bapimandt.
FIELD-SYMBOLS: <range> LIKE clients.

GET REFERENCE OF bapiclient INTO range_reference.
"default SIGN = I, OPTION = EQ
range_reference = zcl_data_tool_pre74=>convert_domain_to_range( i_domain_reference = range_reference ). 
ASSIGN range_reference->* TO <range>.
```
* passing domain name
```ABAP
DATA: range_reference TYPE REF TO data,
      clients         TYPE RANGE OF bapimandt.
FIELD-SYMBOLS: <range> LIKE clients.

range_reference = zcl_data_tool_pre74=>convert_domain_to_range( i_domain_name = 'MANDT' ). "default SIGN = I, OPTION = EQ
ASSIGN range_reference->* TO <range>.
```

