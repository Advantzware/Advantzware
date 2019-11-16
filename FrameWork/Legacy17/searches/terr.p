/* terr.p - Generated 01/13/2000 -  9:38 am by nosweat
"terr. " ~
"ASI " ~
"terr " ~
"terr.company = gcompany " ~
"terr " ~
"4 " ~
"17 " ~
"46 " ~
"terr,dscr " ~
"Territory,Description " ~
"yes " ~
"terr,dscr " ~
"dscr " ~
"Territory Search " ~
"{custom/getcmpny.i} " ~
"{custom/gcompany.i} " ~
" " ~
"terr. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file terr
&Scoped-define where-statement terr.company = gcompany
&Scoped-define return-field terr
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 46
&Scoped-define show-fields terr.terr terr.dscr
&Scoped-define frame-title Territory Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 26
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname terr.
&Scoped-define window-size 23
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 33
&Scoped-define btn-cancel-col 22
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(3)
&Scoped-define FLDNAME1 terr.terr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Territory
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 terr.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD dscr

{methods/search.i}
