/* prod.p - Generated 01/26/2000 -  6:03 pm by nosweat
"prod. " ~
"ASI " ~
"prod " ~
"prod.company = gcompany " ~
"prolin " ~
"4 " ~
"17 " ~
"63 " ~
"prolin,dscr " ~
"Product Line,Description " ~
"no " ~
"prolin,dscr " ~
"dscr " ~
"Product Line GL Accounts Search " ~
"{custom/getcmpny.i} " ~
"{custom/gcompany.i} " ~
" " ~
"prod. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file prod
&Scoped-define where-statement prod.company = gcompany
&Scoped-define return-field prolin
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 63
&Scoped-define show-fields prod.prolin prod.dscr
&Scoped-define frame-title Product Line GL Accounts Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 43
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname prod.
&Scoped-define window-size 23
&Scoped-define window-col 43.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 57
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 50
&Scoped-define btn-cancel-col 39
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(8)
&Scoped-define FLDNAME1 prod.prolin
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Product Line
&Global-define FORMAT-2 x(45)
&Scoped-define FLDNAME2 prod.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD dscr

{methods/search.i}
