/* uom.p - Generated 01/17/2000 -  7:00 pm by nosweat
"uom. " ~
"ASI " ~
"uom " ~
" " ~
"uom " ~
"4 " ~
"19 " ~
"41 " ~
"uom,dscr " ~
"UOM,Description " ~
"yes " ~
"uom,dscr " ~
"Unit of Measure Lookups " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"uom. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file uom
&Scoped-define where-statement TRUE
&Scoped-define return-field uom
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 41
&Scoped-define show-fields uom.uom uom.dscr
&Scoped-define frame-title Unit of Measure Lookups
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
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 32
&Scoped-define btn-cancel-col 21
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(4)
&Scoped-define FLDNAME1 uom.uom
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 UOM
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 uom.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
