/* terms_.p - Generated 01/17/2000 - 12:11 pm by nosweat
"terms_. " ~
"ASI " ~
"terms " ~
"terms.company = gcompany " ~
"dscr " ~
"4 " ~
"19 " ~
"42 " ~
"dscr,t-code " ~
"Description,Terms " ~
"yes " ~
"dscr,t-code " ~
"Terms Description Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"terms. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file terms
&Scoped-define where-statement terms.company = gcompany
&Scoped-define return-field dscr
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 42
&Scoped-define show-fields terms.dscr terms.t-code
&Scoped-define frame-title Terms Description Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname terms.
&Scoped-define window-size 23
&Scoped-define window-col 54
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 36
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 33
&Scoped-define btn-cancel-col 22
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(30)
&Scoped-define FLDNAME1 terms.dscr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 x(5)
&Scoped-define FLDNAME2 terms.t-code
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Terms

{methods/lookup.i}
