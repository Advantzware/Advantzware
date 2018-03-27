/* ventype_.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"ventype_. " ~
"ASI " ~
"ventype " ~
"ventype.company = gcompany " ~
"Dscr " ~
"4 " ~
"19 " ~
"48 " ~
"Dscr,type " ~
"Description,Vendor Type " ~
"yes " ~
"Dscr,type " ~
"Vendor Types Description Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"ventype. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file ventype
&Scoped-define where-statement ventype.company = gcompany
&Scoped-define return-field Dscr
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 48
&Scoped-define show-fields ventype.Dscr ventype.type
&Scoped-define show-fields-yellow ventype.Dscr LABEL-BGCOLOR 14 ventype.type LABEL-BGCOLOR 14
&Scoped-define frame-title Vendor Types Description Lookup
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

&Global-define FORMAT-1 x(30)
&Scoped-define FLDNAME1 ventype.Dscr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 x(8)
&Scoped-define FLDNAME2 ventype.type
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Vendor Type

{methods/lookup.i}
