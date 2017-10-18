/* uom_.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"uom_. " ~
"ASI " ~
"uom " ~
" " ~
"dscr " ~
"4 " ~
"19 " ~
"41 " ~
"dscr,uom " ~
"Description,UOM " ~
"yes " ~
"dscr,uom " ~
"Unit of Measure Description Lookups " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"uom. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file uom
&Scoped-define where-statement TRUE
&Scoped-define return-field dscr
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 41
&Scoped-define show-fields uom.dscr uom.uom
&Scoped-define show-fields-yellow uom.dscr LABEL-BGCOLOR 14 uom.uom LABEL-BGCOLOR 14
&Scoped-define frame-title Unit of Measure Description Lookups
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname uom.
&Scoped-define window-size 24
&Scoped-define window-col 54.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 35
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 34
&Scoped-define btn-cancel-col 27
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(30)
&Scoped-define FLDNAME1 uom.dscr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 x(4)
&Scoped-define FLDNAME2 uom.uom
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 UOM

{methods/lookup.i}
