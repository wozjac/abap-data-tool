# ABAP Data Tools
Various ABAP utilities.

## Classes
### ZCL_DATA_TOOL and ZCL_DATA_TOOL_PRE74
Both classes deliver the same functionality, but the former is for NW system version > 7.4 (written with new syntax), while the latter is dedicated for NW system version < 7.4. 
#### Methods:
##### convert_int_table_to_range
Converts an internal table to a range. 

Features:
- supports non- and structured line type
- works for elementary types (or DDIC types based on): p, c, n, d, t, f, i, int8, string, decfloat16, decfloat34  

Usage:  
The internal table has to be provided as data reference and the final range is returned as reference too.
If the table is non-structured,*i_low_fieldname* and *i_high_fieldname* are not relevant. If the table is structured, at least one field name of the structure has to be provided.

Examples: 
* non-structured table (using ZCL_DATA_TOOL)

```ABAP
TYPES: ty_table TYPE STANDARD TABLE OF i WITH EMPTY KEY.

DATA(table) = VALUE ty_table( ( 1 ) ( 2 ) ( 3 ) ).
"default SIGN = I, OPTION = EQ
DATA(range) = zcl_data_tool=>convert_int_table_to_range( i_table = REF #( table ) ).
ASSIGN range->* TO FIELD-SYMBOL(<range>).
```

* structured table (using ZCL_DATA_TOOL)


```ABAP
TYPES: BEGIN OF ty_data,
         name  TYPE string,
         value TYPE i,
       END OF ty_data.
TYPES: ty_table TYPE STANDARD TABLE OF ty_data WITH EMPTY KEY.

DATA(table) = VALUE ty_table(
  (
    name = 'abc'
    value = 1
  )
  (
    name = 'bca'
    value = 2
  )
).

"default SIGN = I, OPTION = EQ
DATA(range) = zcl_data_tool=>convert_int_table_to_range(
  i_table = REF #( table )
  i_low_fieldname = 'VALUE'
).

ASSIGN range->* TO FIELD-SYMBOL(<range>).
```

* non-structured table (using ZCL_DATA_TOOL_PRE74)

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
ref_range = zcl_data_tool_pre74=>convert_int_table_to_range( i_table = ref_table ). 
ASSIGN ref_range->* to <range>.
```  

* structured table (using ZCL_DATA_TOOL_PRE74)

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

ref_range = zcl_data_tool_pre74=>convert_int_table_to_range( 
  i_table = ref_table 
  i_low_fieldname = 'VALUE' 
  i_sign = 'E' 
).

ASSIGN ref_range->* TO <range>.
```  
##### convert_domain_to_range
Converts a domain (provided as a name or taken from a data object) to a range.  

Features:
* supports domains with fixed values and value tables
* supported types: see documentation *convert_int_table_to_range*
    
Usage:  
pass a domain name or a reference to a data object with domain. The result range is returned as a data reference.

Examples:
* passing domain name (using ZCL_DATA_TOOL)

```ABAP
TYPES: ty_range TYPE RANGE OF bapimandt.
FIELD-SYMBOLS: <range> TYPE ty_range.

"default SIGN = I, OPTION = EQ
DATA(range) = zcl_data_tool_pre74=>convert_domain_to_range( i_domain_name = 'MANDT' ).
ASSIGN range->* TO <range>.
```

* passing a data object with a domain type (using ZCL_DATA_TOOL)

```ABAP
TYPES: ty_clients TYPE RANGE OF bapimandt.
DATA: bapiclient      TYPE bapimandt.
FIELD-SYMBOLS: <range> TYPE ty_clients.

"default SIGN = I, OPTION = EQ
DATA(range) = zcl_data_tool_pre74=>convert_domain_to_range( i_domain_reference = REF #( bapiclient ) ).
ASSIGN range->* TO <range>.
```

* passing a data object with a domain type (using ZCL_DATA_TOOL_PRE74)

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

* passing domain name (using ZCL_DATA_TOOL_PRE74)

```ABAP
DATA: range_reference TYPE REF TO data,
      clients         TYPE RANGE OF bapimandt.
FIELD-SYMBOLS: <range> LIKE clients.

range_reference = zcl_data_tool_pre74=>convert_domain_to_range( i_domain_name = 'MANDT' ). "default SIGN = I, OPTION = EQ
ASSIGN range_reference->* TO <range>.
```

## License
This extension is licensed under the [MIT license](http://opensource.org/licenses/MIT).

## Author
Feel free to contact me:  
- wozjac@zoho.com 
- Twitter (https://twitter.com/jacekwoz)  
- LinkedIn (https://www.linkedin.com/in/jacek-wznk)
