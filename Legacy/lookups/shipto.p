/* shipto.p - Generated 01/29/2003 - 11:19 am by nosweat
"shipto. " ~
"ASI " ~
"shipto " ~
" " ~
"ship-id " ~
"3 " ~
"19 " ~
"65 " ~
"ship-id,ship-name " ~
"Ship To ID,Name " ~
"yes " ~
"ship-id,ship-name " ~
"Customers Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared} " ~
" " ~
"shipto. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file shipto
&Scoped-define where-statement shipto.company eq cocode
&Scoped-define return-field ship-id
&Scoped-define font 3
&Scoped-define height-size 19
&Scoped-define width-size 65
&Scoped-define show-fields shipto.ship-id shipto.ship-name
&Scoped-define frame-title Customers Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname shipto.
&Scoped-define window-size 23
&Scoped-define window-col 42.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 59
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 56
&Scoped-define btn-cancel-col 45
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(8)
&Scoped-define FLDNAME1 shipto.ship-id
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Ship To ID
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 shipto.ship-name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Name

{methods/lookup.i}
