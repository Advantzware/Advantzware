/* mat.p - Generated 01/18/2000 - 10:57 am by nosweat
"mat. " ~
"ASI " ~
"mat " ~
"mat.company = gcompany " ~
"mat " ~
"4 " ~
"17 " ~
"41 " ~
"mat,dscr " ~
"Type,Description " ~
"yes " ~
"mat,dscr " ~
"dscr " ~
"Material Types Search " ~
"{custom/getcmpny.i} " ~
"{custom/gcompany.i} " ~
" " ~
"mat. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file mat
&Scoped-define where-statement mat.company = gcompany
&Scoped-define return-field mat
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 41
&Scoped-define show-fields mat.mat mat.dscr
&Scoped-define frame-title Material Types Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 21
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname mat.
&Scoped-define window-size 23
&Scoped-define window-col 54.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 35
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 28
&Scoped-define btn-cancel-col 17
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 mat.mat
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Type
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 mat.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD dscr

{methods/search.i}
