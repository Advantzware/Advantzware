/* shifts.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"shifts. " ~
"ASI " ~
"shifts " ~
"shifts.company EQ gcompany " ~
"shift " ~
"4 " ~
"19 " ~
"46 " ~
"shift,description " ~
"Shift,Description " ~
"yes " ~
"shift,description " ~
"Shifts Loopup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file shifts
&Scoped-define where-statement shifts.company EQ gcompany
&Scoped-define return-field shift
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields shifts.shift shifts.description
&Scoped-define show-fields-yellow shifts.shift LABEL-BGCOLOR 14 shifts.description LABEL-BGCOLOR 14
&Scoped-define frame-title Shifts Loopup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 24
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 39
&Scoped-define btn-cancel-col 32
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 X
&Scoped-define FLDNAME1 shifts.shift
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Shift
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 shifts.description
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
