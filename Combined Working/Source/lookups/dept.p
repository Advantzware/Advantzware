/* dept.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"dept. " ~
"ASI " ~
"dept " ~
" " ~
"code " ~
"2 " ~
"19 " ~
"53 " ~
"code,dscr,fc " ~
"Code,Description,Sequence " ~
"no " ~
"code,dscr,fc " ~
"Departments Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
"dept. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file dept
&Scoped-define where-statement TRUE
&Scoped-define return-field code
&Scoped-define font 2
&Scoped-define height-size 19
&Scoped-define width-size 53
&Scoped-define show-fields dept.code dept.dscr dept.fc
&Scoped-define show-fields-yellow dept.code LABEL-BGCOLOR 14 dept.dscr LABEL-BGCOLOR 14 dept.fc LABEL-BGCOLOR 14
&Scoped-define frame-title Departments Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname dept.
&Scoped-define window-size 24
&Scoped-define window-col 48.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 47
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 46
&Scoped-define btn-cancel-col 39
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(2)
&Scoped-define FLDNAME1 dept.code
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Code
&Global-define FORMAT-2 x(25)
&Scoped-define FLDNAME2 dept.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Global-define DATATYP3 INTEGER
&Global-define FORMAT-3 >9
&Scoped-define FLDNAME3 dept.fc
&Scoped-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-2}
&Scoped-define DESCRIP3 Sequence

{methods/lookup.i}
