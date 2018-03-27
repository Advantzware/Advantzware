/* prep.p - Generated 01/17/2000 - 12:02 pm by nosweat
"prep. " ~
"ASI " ~
"prep " ~
"prep.company = gcompany " ~
"code " ~
"4 " ~
"17 " ~
"42 " ~
"code,dscr,mat-type " ~
"Code,Description " ~
"yes " ~
"code,dscr " ~
"dscr " ~
"Preparation File Search " ~
"{custom/getcmpny.i} ~{custom/getloc.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} " ~
" " ~
"prep. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file prep
&Scoped-define where-statement prep.company = gcompany
&Scoped-define return-field code
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 42
&Scoped-define show-fields prep.code prep.dscr prep.mat-type
&Scoped-define frame-title Preparation File Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 22
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname prep.
&Scoped-define window-size 23
&Scoped-define window-col 54
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 36
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 29
&Scoped-define btn-cancel-col 18
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 prep.code
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Code
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 prep.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD dscr

{methods/search.i}
