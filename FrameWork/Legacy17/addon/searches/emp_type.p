/* emp_type.p - Generated 03/30/2000 - 10:25 am by nosweat
"emp_type. " ~
"EMPTRACK " ~
"emp_type " ~
" " ~
"emp_type " ~
"4 " ~
"10 " ~
"40 " ~
"emp_type,description " ~
"Employee Type,Description " ~
"yes " ~
"emp_type,description " ~
"description " ~
"Employee Types Search " ~
" ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
" ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
"emp_type. " ~
*/

&Scoped-define search-db 
&Scoped-define search-file emp_type
&Scoped-define where-statement TRUE
&Scoped-define return-field emp_type
&Scoped-define font 4
&Scoped-define height-size 10
&Scoped-define width-size 40
&Scoped-define show-fields emp_type.emp_type emp_type.description
&Scoped-define frame-title Employee Types Search
&Scoped-define search-text-row 11
&Scoped-define word-search-row 12
&Scoped-define btn-search-col 20
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} 
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname emp_type.
&Scoped-define window-size 16
&Scoped-define window-col 55
&Scoped-define rect-1-row 13.15
&Scoped-define by-row 13.42
&Scoped-define browse-order-width 34
&Scoped-define browse-order-row 13.42
&Scoped-define btn-row 14.77
&Scoped-define btn-ok-col 27
&Scoped-define btn-cancel-col 16
&Scoped-define auto-find-row 15.85

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 emp_type.emp_type
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Employee Type
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 emp_type.description
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD description

{methods/search.i}
