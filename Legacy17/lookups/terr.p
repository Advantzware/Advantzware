/* terr.p - Generated 01/24/2000 -  5:23 pm by nosweat
"terr. " ~
"ASI " ~
"terr " ~
"terr.company = gcompany " ~
"terr " ~
"4 " ~
"19 " ~
"40 " ~
"terr,dscr " ~
"Territory,Description " ~
"yes " ~
"terr,dscr " ~
"Territory Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"terr. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file terr
&Scoped-define where-statement terr.company = gcompany
&Scoped-define return-field terr
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 40
&Scoped-define show-fields terr.terr terr.dscr
&Scoped-define frame-title Territory Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname terr.
&Scoped-define window-size 23
&Scoped-define window-col 55
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 34
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 31
&Scoped-define btn-cancel-col 20
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(3)
&Scoped-define FLDNAME1 terr.terr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Territory
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 terr.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
