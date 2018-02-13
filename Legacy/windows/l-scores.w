/* scores.p - Generated 02/05/2018 -  4:05 pm by NoSweat
"scores. " ~
"ASI " ~
"scores " ~
"scores.company EQ gcompany " ~
"type " ~
"4 " ~
"19 " ~
"46 " ~
"type,dscr " ~
"Score Type,Description " ~
"yes " ~
"type,dscr " ~
"Score Types Lookup " ~
"{custom/yellowColumns.i} ~{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/account.i} ~{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file scores
&Scoped-define where-statement scores.company EQ gcompany
&Scoped-define return-field type
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields scores.type scores.dscr
&Scoped-define show-fields-yellow scores.type LABEL-BGCOLOR 14 scores.dscr LABEL-BGCOLOR 14
&Scoped-define frame-title Score Types Lookup
&Scoped-define top-include ~{custom/yellowColumns.i} ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/account.i} ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
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

&Global-define FORMAT-1 x
&Scoped-define FLDNAME1 scores.type
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Score Type
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 scores.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
