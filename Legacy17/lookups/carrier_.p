/* carrier_.p - Generated 01/17/2000 - 12:15 pm by nosweat
"carrier_. " ~
"ASI " ~
"carrier " ~
"carrier.company = gcompany " ~
"dscr " ~
"4 " ~
"19 " ~
"42 " ~
"dscr,carrier " ~
"Description,Carrier " ~
"yes " ~
"dscr,carrier " ~
"Common Carriers Description Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"carrier. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file carrier
&Scoped-define where-statement carrier.company = gcompany
&Scoped-define return-field dscr
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 42
&Scoped-define show-fields carrier.dscr carrier.carrier
&Scoped-define frame-title Common Carriers Description Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname carrier.
&Scoped-define window-size 24
&Scoped-define window-col 54
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 36
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 33
&Scoped-define btn-cancel-col 22
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(30)
&Scoped-define FLDNAME1 carrier.dscr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 x(5)
&Scoped-define FLDNAME2 carrier.carrier
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Carrier

{methods/lookup.i}
