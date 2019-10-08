/* rejct-cd.p - Generated 01/18/2000 -  5:22 pm by nosweat
"rejct-cd. " ~
"ASI " ~
"rejct-cd " ~
" " ~
"code " ~
"4 " ~
"17 " ~
"56 " ~
"code,dscr " ~
"Code,Description " ~
"yes " ~
"code,dscr " ~
"dscr " ~
"PO Rejection Codes Search " ~
" " ~
" " ~
" " ~
"rejct-cd. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file rejct-cd
&Scoped-define where-statement TRUE
&Scoped-define return-field code
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 56
&Scoped-define show-fields rejct-cd.code rejct-cd.dscr
&Scoped-define frame-title PO Rejection Codes Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 36
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared} 
&Scoped-define end-include 
&Scoped-define ui-prgmname rejct-cd.
&Scoped-define window-size 23
&Scoped-define window-col 47
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 50
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 43
&Scoped-define btn-cancel-col 32
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(2)
&Scoped-define FLDNAME1 rejct-cd.code
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Code
&Global-define FORMAT-2 x(45)
&Scoped-define FLDNAME2 rejct-cd.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD dscr

{methods/search.i}
