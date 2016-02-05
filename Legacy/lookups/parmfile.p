/* parmfile.p - Generated 07/25/1998 -  1:19 am by NoSweat
"parmfile. " ~
"NOSWEAT " ~
"parmfile " ~
" " ~
"parmfile " ~
"4 " ~
"10 " ~
"46 " ~
"parmfile " ~
"Parameter File " ~
"yes " ~
"parmfile " ~
"Parameter File Lookup " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"parmfile. " ~
*/

&Scoped-define lookup-db NOSWEAT.
&Scoped-define lookup-file parmfile
&Scoped-define where-statement TRUE
&Scoped-define return-field parmfile
&Scoped-define font 4
&Scoped-define height-size 10
&Scoped-define width-size 46
&Scoped-define show-fields parmfile.parmfile
&Scoped-define frame-title Parameter File Lookup
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname parmfile.
&Scoped-define window-size 14
&Scoped-define window-col 52
&Scoped-define rect-1-row 11.15
&Scoped-define by-row 11.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 11.42
&Scoped-define btn-row 12.77
&Scoped-define btn-ok-col 37
&Scoped-define btn-cancel-col 26
&Scoped-define auto-find-row 13.85

&Global-define FORMAT-1 X(40)
&Scoped-define FLDNAME1 parmfile.parmfile
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Parameter File

{methods/lookup.i}
