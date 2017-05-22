/* terms.p - Generated 01/13/2000 -  9:37 am by nosweat
"terms. " ~
"ASI " ~
"terms " ~
"terms.company = gcompany " ~
"t-code " ~
"4 " ~
"17 " ~
"46 " ~
"t-code,dscr " ~
"Terms,Description " ~
"yes " ~
"t-code,dscr " ~
"dscr " ~
"Terms Search " ~
"{custom/getcmpny.i} " ~
"{custom/gcompany.i} " ~
" " ~
"terms. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file terms
&Scoped-define where-statement terms.company = gcompany
&Scoped-define return-field t-code
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 46
&Scoped-define show-fields terms.t-code terms.dscr
&Scoped-define frame-title Terms Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 26
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname terms.
&Scoped-define window-size 23
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 33
&Scoped-define btn-cancel-col 22
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 terms.t-code
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Terms
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 terms.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD dscr

{methods/search.i}
