/* job-code.p - Generated 05/04/2000 -  7:05 pm by nosweat
"job-code. " ~
"ASI " ~
"job-code " ~
" " ~
"code " ~
"4 " ~
"10 " ~
"65 " ~
"code,cat,dscr " ~
"Code,Category,Description " ~
"yes " ~
"code,cat,dscr " ~
"Charge Code Lookups " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file job-code
&Scoped-define where-statement TRUE
&Scoped-define return-field code
&Scoped-define font 4
&Scoped-define height-size 10
&Scoped-define width-size 65
&Scoped-define show-fields job-code.code job-code.cat job-code.dscr
&Scoped-define frame-title Charge Code Lookups
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 14
&Scoped-define window-col 42.5
&Scoped-define rect-1-row 11.15
&Scoped-define by-row 11.42
&Scoped-define browse-order-width 59
&Scoped-define browse-order-row 11.42
&Scoped-define btn-row 12.77
&Scoped-define btn-ok-col 56
&Scoped-define btn-cancel-col 45
&Scoped-define auto-find-row 13.85

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

{methods/lookup.i}
