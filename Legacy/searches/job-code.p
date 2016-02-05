/* job-code.p - Generated 01/18/2000 -  5:39 pm by nosweat
"job-code. " ~
"ASI " ~
"job-code " ~
" " ~
"code " ~
"4 " ~
"17 " ~
"65 " ~
"code,cat,dscr " ~
"Code,Category,Description " ~
"yes " ~
"code,cat,dscr " ~
"dscr " ~
"Machine Charge Codes Search " ~
" " ~
" " ~
" " ~
"job-code. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file job-code
&Scoped-define where-statement TRUE
&Scoped-define return-field code
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 65
&Scoped-define show-fields job-code.code job-code.cat job-code.dscr
&Scoped-define frame-title Machine Charge Codes Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 45
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname job-code.
&Scoped-define window-size 23
&Scoped-define window-col 42.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 59
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 52
&Scoped-define btn-cancel-col 41
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 job-code.code
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Code
&Global-define FORMAT-2 x(3)
&Scoped-define FLDNAME2 job-code.cat
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Category
&Global-define FORMAT-3 x(45)
&Scoped-define FLDNAME3 job-code.dscr
&Scoped-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-2}
&Scoped-define DESCRIP3 Description
&Scoped-define WORDFLD dscr

{methods/search.i}
