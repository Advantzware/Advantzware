/* shifts.p - Generated 03/30/2000 - 10:43 am by nosweat
"shifts. " ~
"EMPTRACK " ~
"shifts " ~
" " ~
"shift " ~
"2 " ~
"10 " ~
"45 " ~
"shift,description " ~
"Shift,Description " ~
"yes " ~
"shift,description " ~
"description " ~
"Shifts Search " ~
" ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
" ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
"shifts. " ~
*/

&Scoped-define search-db 
&Scoped-define search-file shifts
&Scoped-define where-statement TRUE
&Scoped-define return-field shift
&Scoped-define font 2
&Scoped-define height-size 10
&Scoped-define width-size 45
&Scoped-define show-fields shifts.shift shifts.description
&Scoped-define frame-title Shifts Search
&Scoped-define search-text-row 11
&Scoped-define word-search-row 12
&Scoped-define btn-search-col 25
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} 
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} 
&Scoped-define end-include 
&Scoped-define ui-prgmname shifts.
&Scoped-define window-size 16
&Scoped-define window-col 52.5
&Scoped-define rect-1-row 13.15
&Scoped-define by-row 13.42
&Scoped-define browse-order-width 39
&Scoped-define browse-order-row 13.42
&Scoped-define btn-row 14.77
&Scoped-define btn-ok-col 32
&Scoped-define btn-cancel-col 21
&Scoped-define auto-find-row 15.85

&Global-define FORMAT-1 X(8)
&Scoped-define FLDNAME1 shifts.shift
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Shift
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 shifts.description
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD description

{methods/search.i}
