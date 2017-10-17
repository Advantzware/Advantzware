/* shipto.p - Generated 09/20/2000 -  9:59 am by nosweat
"shipto. " ~
"ASI " ~
"shipto " ~
"shipto.company = gcompany and shipto.cust-no = s-cust-no " ~
"ship-id " ~
"2 " ~
"19 " ~
"149 " ~
"ship-id,ship-name,ship-addr[1],ship-city,ship-state,ship-zip " ~
"Ship To ID,Name " ~
"yes " ~
"ship-id,ship-name " ~
" " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file shipto
&Scoped-define where-statement shipto.company = gcompany
&Scoped-define return-field ship-id
&Scoped-define font 2
&Scoped-define height-size 19
&Scoped-define width-size 149
&Scoped-define show-fields shipto.ship-id shipto.ship-name shipto.ship-addr[1] shipto.ship-city shipto.ship-state shipto.ship-zip
&Scoped-define frame-title 
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 24
&Scoped-define window-col .5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 143
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 140
&Scoped-define btn-cancel-col 129
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(8)
&Scoped-define FLDNAME1 shipto.ship-id
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Ship To ID
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 shipto.ship-name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Name

{methods/lookup.i}
