/* emp_typ_.p - Generated 03/30/2000 - 10:35 am by nosweat
"emp_typ_. " ~
"EMPTRACK " ~
"emp_type " ~
" " ~
"description " ~
"4 " ~
"10 " ~
"40 " ~
"description,emp_type " ~
"Description,Employee Type " ~
"yes " ~
"description,emp_type " ~
"Employee Types Description Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i}" ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"emp_type. " ~
*/

&Scoped-define lookup-db 
&Scoped-define lookup-file emp_type
&Scoped-define where-statement TRUE
&Scoped-define return-field description
&Scoped-define font 4
&Scoped-define height-size 10
&Scoped-define width-size 40
&Scoped-define show-fields emp_type.description emp_type.emp_type
&Scoped-define frame-title Employee Types Description Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname emp_type.
&Scoped-define window-size 14
&Scoped-define window-col 55
&Scoped-define rect-1-row 11.15
&Scoped-define by-row 11.42
&Scoped-define browse-order-width 34
&Scoped-define browse-order-row 11.42
&Scoped-define btn-row 12.77
&Scoped-define btn-ok-col 31
&Scoped-define btn-cancel-col 20
&Scoped-define auto-find-row 13.85

&Global-define FORMAT-1 x(20)
&Scoped-define FLDNAME1 emp_type.description
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 x(5)
&Scoped-define FLDNAME2 emp_type.emp_type
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Employee Type

{methods/lookup.i}
