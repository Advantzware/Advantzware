/* prep_.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"prep_. " ~
"ASI " ~
"prep " ~
"prep.company = gcompany " ~
"dscr " ~
"4 " ~
"19 " ~
"42 " ~
"dscr,code,mat-type " ~
"Description,Code " ~
"yes " ~
"dscr,code " ~
"Preparation File Description Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared} " ~
" " ~
"prep. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file prep
&Scoped-define where-statement prep.company = gcompany
&Scoped-define return-field dscr
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 42
&Scoped-define show-fields prep.dscr prep.code prep.mat-type
&Scoped-define show-fields-yellow prep.dscr LABEL-BGCOLOR 14 prep.code LABEL-BGCOLOR 14 prep.mat-type LABEL-BGCOLOR 14
&Scoped-define frame-title Preparation File Description Lookup
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
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 35
&Scoped-define btn-cancel-col 28
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(20)
&Scoped-define FLDNAME1 prep.dscr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 x(5)
&Scoped-define FLDNAME2 prep.code
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Code

{methods/lookup.i}
