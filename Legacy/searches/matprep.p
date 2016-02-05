/* matprep.p - Generated 01/18/2000 -  9:49 am by nosweat
"matprep. " ~
"ASI " ~
"matprep " ~
"matprep.company = gcompany " ~
"mat " ~
"4 " ~
"17 " ~
"41 " ~
"mat,dscr " ~
"Type,Description " ~
"yes " ~
"mat,dscr " ~
"dscr " ~
"Preparation Material Codes Search " ~
"{custom/getcmpny.i} " ~
"{custom/gcompany.i} " ~
" " ~
"matprep. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file matprep
&Scoped-define where-statement matprep.company = gcompany
&Scoped-define return-field mat
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 41
&Scoped-define show-fields matprep.mat matprep.dscr
&Scoped-define frame-title Preparation Material Codes Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 21
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname matprep.
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
&Scoped-define FLDNAME1 matprep.mat
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Type
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 matprep.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD dscr

{methods/search.i}
