/* prgmxref.p - Generated 05/15/1998 -  3:13 pm by NOSWEAT
"prgmxref. " ~
"NOSWEAT " ~
"prgmxref " ~
" " ~
"table_name " ~
"4 " ~
"19 " ~
"46 " ~
"table_name,prgmname " ~
"Table Name " ~
"yes " ~
"table_name " ~
"Program Xref Lookup " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"prgmxref. " ~
*/

&Scoped-define lookup-db NOSWEAT.
&Scoped-define lookup-file prgmxref
&Scoped-define where-statement TRUE
&Scoped-define return-field table_name
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields prgmxref.table_name prgmxref.prgmname
&Scoped-define frame-title Program Xref Lookup
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname prgmxref.
&Scoped-define window-size 23
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 37
&Scoped-define btn-cancel-col 26
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 X(8)
&Scoped-define FLDNAME1 prgmxref.table_name
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Table Name

{methods/lookup.i}
