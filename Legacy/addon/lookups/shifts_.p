/* shifts_.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"shifts_. " ~
"ASI " ~
"shifts " ~
"shifts.company = gcompany " ~
"description " ~
"2 " ~
"10 " ~
"45 " ~
"description,shift " ~
"Description,Shift " ~
"yes " ~
"description,shift " ~
"Shifts Description Lookup " ~
" ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}" ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared}" ~
" " ~
"shifts. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file shifts
&Scoped-define where-statement shifts.company = gcompany
&Scoped-define return-field description
&Scoped-define font 2
&Scoped-define height-size 10
&Scoped-define width-size 45
&Scoped-define show-fields shifts.description shifts.shift
&Scoped-define show-fields-yellow shifts.description LABEL-BGCOLOR 14 shifts.shift LABEL-BGCOLOR 14
&Scoped-define frame-title Shifts Description Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname shifts.
&Scoped-define window-size 15
&Scoped-define window-col 52.5
&Scoped-define rect-1-row 11.15
&Scoped-define by-row 11.42
&Scoped-define browse-order-width 39
&Scoped-define browse-order-row 11.42
&Scoped-define btn-row 12.7
&Scoped-define btn-ok-col 38
&Scoped-define btn-cancel-col 31
&Scoped-define auto-find-row 14.65

&Global-define FORMAT-1 x(20)
&Scoped-define FLDNAME1 shifts.description
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 X
&Scoped-define FLDNAME2 shifts.shift
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Shift

{methods/lookup.i}
