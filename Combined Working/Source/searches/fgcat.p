/* fgcat.p - Generated 01/19/2000 - 11:47 am by nosweat
"fgcat. " ~
"ASI " ~
"fgcat " ~
"fgcat.company = gcompany " ~
"procat " ~
"4 " ~
"17 " ~
"38 " ~
"procat,dscr " ~
"Category,Description " ~
"yes " ~
"procat,dscr " ~
"dscr " ~
"Finished Goods Categories Search " ~
"{custom/getcmpny.i} " ~
"{custom/gcompany.i} " ~
" " ~
"fgcat. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file fgcat
&Scoped-define where-statement fgcat.company = gcompany
&Scoped-define return-field procat
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 38
&Scoped-define show-fields fgcat.procat fgcat.dscr
&Scoped-define frame-title Finished Goods Categories Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 18
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname fgcat.
&Scoped-define window-size 23
&Scoped-define window-col 56
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 32
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 25
&Scoped-define btn-cancel-col 14
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 fgcat.procat
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Category
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 fgcat.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD dscr

{methods/search.i}
