/* loc_.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"loc_. " ~
"ASI " ~
"loc " ~
"loc.company = gcompany " ~
"dscr " ~
"4 " ~
"19 " ~
"44 " ~
"dscr,loc " ~
"Description,Location " ~
"yes " ~
"dscr,loc " ~
"Location Description Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"loc. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file loc
&Scoped-define where-statement loc.company = gcompany
&Scoped-define return-field dscr
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 44
&Scoped-define show-fields loc.dscr loc.loc
&Scoped-define show-fields-yellow loc.dscr LABEL-BGCOLOR 14 loc.loc LABEL-BGCOLOR 14
&Scoped-define frame-title Location Description Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname loc.
&Scoped-define window-size 24
&Scoped-define window-col 53
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 38
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 37
&Scoped-define btn-cancel-col 30
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(30)
&Scoped-define FLDNAME1 loc.dscr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 x(5)
&Scoped-define FLDNAME2 loc.loc
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Location

{methods/lookup.i}
