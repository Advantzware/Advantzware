/* dept.p - Generated 01/19/2000 -  9:29 pm by nosweat
"dept. " ~
"ASI " ~
"dept " ~
" " ~
"code " ~
"4 " ~
"17 " ~
"47 " ~
"code,dscr,fc " ~
"Code,Description " ~
"yes " ~
"code,dscr " ~
"dscr " ~
"Departments Search " ~
" " ~
" " ~
" " ~
"dept. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file dept
&Scoped-define where-statement TRUE
&Scoped-define return-field code
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 47
&Scoped-define show-fields dept.code dept.dscr dept.fc
&Scoped-define frame-title Departments Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 27
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname dept.
&Scoped-define window-size 23
&Scoped-define window-col 51.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 41
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 34
&Scoped-define btn-cancel-col 23
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(2)
&Scoped-define FLDNAME1 dept.code
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Code
&Global-define FORMAT-2 x(25)
&Scoped-define FLDNAME2 dept.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD dscr

{methods/search.i}
