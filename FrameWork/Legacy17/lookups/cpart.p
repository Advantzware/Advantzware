/* cpart.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"cpart. " ~
"asi " ~
"oe-ordl " ~
" " ~
"part-no " ~
"3 " ~
"19 " ~
"69 " ~
"part-no,i-name " ~
"Cust Part #,Name " ~
"yes " ~
"part-no,i-name " ~
"Cust Part# Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"cpart. " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file oe-ordl
&Scoped-define where-statement TRUE
&Scoped-define return-field part-no
&Scoped-define font 3
&Scoped-define height-size 19
&Scoped-define width-size 69
&Scoped-define show-fields oe-ordl.part-no oe-ordl.i-name
&Scoped-define show-fields-yellow oe-ordl.part-no LABEL-BGCOLOR 14 oe-ordl.i-name LABEL-BGCOLOR 14
&Scoped-define frame-title Cust Part# Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname cpart.
&Scoped-define window-size 24
&Scoped-define window-col 40.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 63
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 62
&Scoped-define btn-cancel-col 55
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(15)
&Scoped-define FLDNAME1 oe-ordl.part-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Cust Part #
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 oe-ordl.i-name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Name

{methods/lookup.i}
