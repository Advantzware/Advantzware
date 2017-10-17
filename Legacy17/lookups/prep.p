/* prep.p - Generated 08/06/2003 - 10:36 am by nosweat
"prep. " ~
"ASI " ~
"prep " ~
"prep.company = gcompany  " ~
"code " ~
"4 " ~
"19 " ~
"42 " ~
"code,dscr,mat-type " ~
"Code,Description " ~
"yes " ~
"code,dscr " ~
"Preparation File Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared} " ~
" " ~
"prep. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file prep
&Scoped-define where-statement prep.company = gcompany
&Scoped-define return-field code
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 42
&Scoped-define show-fields prep.CODE FORMAT "x(20)" WIDTH 23 prep.dscr WIDTH 25 prep.mat-type
&Scoped-define frame-title Preparation File Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname prep.
&Scoped-define window-size 24
&Scoped-define window-col 54
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 36
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 33
&Scoped-define btn-cancel-col 22
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 prep.CODE 
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Code
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 prep.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
