/* matprep.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"matprep. " ~
"ASI " ~
"matprep " ~
"matprep.company = gcompany " ~
"mat " ~
"4 " ~
"19 " ~
"41 " ~
"mat,dscr " ~
"Type,Description " ~
"yes " ~
"mat,dscr " ~
"Preparation Material Codes Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"matprep. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file matprep
&Scoped-define where-statement matprep.company = gcompany
&Scoped-define return-field mat
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 41
&Scoped-define show-fields matprep.mat matprep.dscr
&Scoped-define show-fields-yellow matprep.mat LABEL-BGCOLOR 14 matprep.dscr LABEL-BGCOLOR 14
&Scoped-define frame-title Preparation Material Codes Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname matprep.
&Scoped-define window-size 24
&Scoped-define window-col 54.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 35
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 34
&Scoped-define btn-cancel-col 27
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 matprep.mat
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Type
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 matprep.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
