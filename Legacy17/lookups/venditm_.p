/* venditm_.p - Generated 08/07/2003 - 12:55 pm by nosweat
"venditm_. " ~
"asi " ~
"po-ordl " ~
" " ~
"i-no " ~
"4 " ~
"19 " ~
"46 " ~
"i-no,vend-i-no " ~
"Item#,Vendor Item # " ~
"yes " ~
"i-no,vend-i-no " ~
"Vendor Item# " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file po-ordl
&Scoped-define where-statement po-ordl.company eq cocode
&Scoped-define return-field i-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields po-ordl.i-no po-ordl.vend-i-no
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
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 37
&Scoped-define btn-cancel-col 26
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(15)
&Scoped-define FLDNAME1 po-ordl.i-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Item#
&Global-define FORMAT-2 x(15)
&Scoped-define FLDNAME2 po-ordl.vend-i-no
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Vendor Item #

{methods/lookup.i}
