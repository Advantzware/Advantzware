/* sman.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"sman. " ~
"ASI " ~
"sman " ~
"sman.company = gcompany " ~
"sman " ~
"4 " ~
"19 " ~
"35 " ~
"sman,sname " ~
"Salesman,Number " ~
"yes " ~
"sman,sman-no " ~
"Saleman Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file sman
&Scoped-define where-statement sman.company = gcompany
&Scoped-define return-field sman
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 35
&Scoped-define show-fields sman.sman sman.sname
&Scoped-define show-fields-yellow sman.sman LABEL-BGCOLOR 14 sman.sname LABEL-BGCOLOR 14
&Scoped-define frame-title Saleman Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 24
&Scoped-define window-col 57.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 29
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 28
&Scoped-define btn-cancel-col 21
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(3)
&Scoped-define FLDNAME1 sman.sman
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Salesman
&Global-define DATATYP2 INTEGER
&Global-define FORMAT-2 >>9
&Scoped-define FLDNAME2 sman.sman-no
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Number

{methods/lookup.i}
