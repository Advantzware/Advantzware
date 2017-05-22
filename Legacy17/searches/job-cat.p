/* job-cat.p - Generated 01/18/2000 - 11:38 am by nosweat
"job-cat. " ~
"ASI " ~
"job-cat " ~
" " ~
"cat " ~
"4 " ~
"17 " ~
"59 " ~
"cat,dscr " ~
"Category,Desc " ~
"yes " ~
"cat,dscr " ~
"dscr " ~
"Job Categories Search " ~
" " ~
" " ~
" " ~
"job-cat. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file job-cat
&Scoped-define where-statement TRUE
&Scoped-define return-field cat
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 59
&Scoped-define show-fields job-cat.cat job-cat.dscr
&Scoped-define frame-title Job Categories Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 39
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname job-cat.
&Scoped-define window-size 23
&Scoped-define window-col 45.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 53
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 46
&Scoped-define btn-cancel-col 35
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(3)
&Scoped-define FLDNAME1 job-cat.cat
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Category
&Global-define FORMAT-2 x(45)
&Scoped-define FLDNAME2 job-cat.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Desc
&Scoped-define WORDFLD dscr

{methods/search.i}
