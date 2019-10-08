/* shifts.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"shifts. " ~
"ASI " ~
"shifts " ~
"shifts.company = gcompany " ~
"shift " ~
"2 " ~
"10 " ~
"41 " ~
"shift,description " ~
"Shift,Description " ~
"yes " ~
"shift,description " ~
"Shifts Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"shifts. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file shifts
&Scoped-define where-statement shifts.company = gcompany
&Scoped-define return-field shift
&Scoped-define font 2
&Scoped-define height-size 10
&Scoped-define width-size 41
&Scoped-define show-fields shifts.shift shifts.description
&Scoped-define show-fields-yellow shifts.shift LABEL-BGCOLOR 14 shifts.description LABEL-BGCOLOR 14
&Scoped-define frame-title Shifts Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname shifts.
&Scoped-define window-size 15
&Scoped-define window-col 54.5
&Scoped-define rect-1-row 11.15
&Scoped-define by-row 11.42
&Scoped-define browse-order-width 35
&Scoped-define browse-order-row 11.42
&Scoped-define btn-row 12.7
&Scoped-define btn-ok-col 34
&Scoped-define btn-cancel-col 27
&Scoped-define auto-find-row 14.65

&Global-define FORMAT-1 X
&Scoped-define FLDNAME1 shifts.shift
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Shift
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 shifts.description
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
