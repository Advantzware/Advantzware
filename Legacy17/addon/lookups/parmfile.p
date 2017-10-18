/* parmfile.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"parmfile. " ~
"ASI " ~
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
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
"parmfile. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file parmfile
&Scoped-define where-statement TRUE
&Scoped-define return-field parmfile
&Scoped-define font 4
&Scoped-define height-size 10
&Scoped-define width-size 46
&Scoped-define show-fields parmfile.parmfile
&Scoped-define show-fields-yellow parmfile.parmfile LABEL-BGCOLOR 14
&Scoped-define frame-title Parameter File Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname parmfile.
&Scoped-define window-size 15
&Scoped-define window-col 52
&Scoped-define rect-1-row 11.15
&Scoped-define by-row 11.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 11.42
&Scoped-define btn-row 12.7
&Scoped-define btn-ok-col 39
&Scoped-define btn-cancel-col 32
&Scoped-define auto-find-row 14.65

&Global-define FORMAT-1 X(40)
&Scoped-define FLDNAME1 parmfile.parmfile
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Parameter File

{methods/lookup.i}
