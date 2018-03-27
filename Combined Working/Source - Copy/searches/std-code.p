/* std-code.p - Generated 01/17/2000 -  7:56 pm by nosweat
"std-code. " ~
"ASI " ~
"std-code " ~
"dscr " ~
"4 " ~
"17 " ~
"34 " ~
"code,dscr " ~
"Code,Description " ~
"yes " ~
"code,dscr " ~
"dscr " ~
"Standards Matrix Search " ~
"{custom/getcmpny.i} " ~
"{custom/gcompany.i} " ~
" " ~
"std-code. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file std-code
&Scoped-define where-statement TRUE
&Scoped-define return-field dscr
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 34
&Scoped-define show-fields std-code.code std-code.dscr
&Scoped-define frame-title Standards Matrix Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 14
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname std-code.
&Scoped-define window-size 23
&Scoped-define window-col 58
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 28
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 21
&Scoped-define btn-cancel-col 10
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(2)
&Scoped-define FLDNAME1 std-code.code
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Code
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 std-code.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD dscr

{methods/search.i}
