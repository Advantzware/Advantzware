/* loc.p - Generated 03/20/2003 - 11:28 am by nosweat
"loc. " ~
"ASI " ~
"loc " ~
"loc.company = gcompany " ~
"loc " ~
"4 " ~
"19 " ~
"44 " ~
"loc,dscr " ~
"Location,Description " ~
"yes " ~
"loc,dscr " ~
"Location Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"loc. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file loc
&Scoped-define where-statement loc.company = gcompany
&Scoped-define return-field loc
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 44
&Scoped-define show-fields loc.loc loc.dscr
&Scoped-define frame-title Location Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname loc.
&Scoped-define window-size 23
&Scoped-define window-col 53
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 38
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 35
&Scoped-define btn-cancel-col 24
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 loc.loc
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Location
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 loc.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
