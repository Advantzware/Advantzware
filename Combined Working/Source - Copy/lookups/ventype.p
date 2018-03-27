/* ventype.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"ventype. " ~
"ASI " ~
"ventype " ~
"ventype.company = gcompany " ~
"type " ~
"4 " ~
"19 " ~
"48 " ~
"type,Dscr " ~
"Vendor Type,Description " ~
"yes " ~
"type,Dscr " ~
"Vendor Types Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"ventype. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file ventype
&Scoped-define where-statement ventype.company = gcompany
&Scoped-define return-field type
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 48
&Scoped-define show-fields ventype.type ventype.Dscr
&Scoped-define show-fields-yellow ventype.type LABEL-BGCOLOR 14 ventype.Dscr LABEL-BGCOLOR 14
&Scoped-define frame-title Vendor Types Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname ventype.
&Scoped-define window-size 24
&Scoped-define window-col 51
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 42
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 41
&Scoped-define btn-cancel-col 34
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(8)
&Scoped-define FLDNAME1 ventype.type
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Vendor Type
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 ventype.Dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
