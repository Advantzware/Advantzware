/* std-cod_.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"std-cod_. " ~
"ASI " ~
"std-code " ~
"std-code.company = gcompany " ~
"dscr " ~
"4 " ~
"19 " ~
"34 " ~
"dscr,code " ~
"Description,Code " ~
"yes " ~
"dscr,code " ~
"Standards Matrix Description Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"std-code. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file std-code
&Scoped-define where-statement std-code.company = gcompany
&Scoped-define return-field dscr
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 34
&Scoped-define show-fields std-code.dscr std-code.code
&Scoped-define show-fields-yellow std-code.dscr LABEL-BGCOLOR 14 std-code.code LABEL-BGCOLOR 14
&Scoped-define frame-title Standards Matrix Description Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname std-code.
&Scoped-define window-size 24
&Scoped-define window-col 58
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 28
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 27
&Scoped-define btn-cancel-col 20
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(20)
&Scoped-define FLDNAME1 std-code.dscr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 x(2)
&Scoped-define FLDNAME2 std-code.code
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Code

{methods/lookup.i}
