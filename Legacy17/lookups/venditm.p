/* venditm.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"venditm. " ~
"asi " ~
"po-ordl " ~
" " ~
"vend-i-no " ~
"4 " ~
"19 " ~
"46 " ~
"vend-i-no,i-no " ~
"Vendor Item #,Item# " ~
"yes " ~
"vend-i-no,i-no " ~
"Vendor Item# " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file po-ordl
&Scoped-define where-statement TRUE
&Scoped-define return-field vend-i-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields po-ordl.vend-i-no po-ordl.i-no
&Scoped-define show-fields-yellow po-ordl.vend-i-no LABEL-BGCOLOR 14 po-ordl.i-no LABEL-BGCOLOR 14
&Scoped-define frame-title Vendor Item#
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 24
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 39
&Scoped-define btn-cancel-col 32
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(15)
&Scoped-define FLDNAME1 po-ordl.vend-i-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Vendor Item #
&Global-define FORMAT-2 x(15)
&Scoped-define FLDNAME2 po-ordl.i-no
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Item#

{methods/lookup.i}
