/* mach1.p - Generated 07/28/2017 - 12:48 pm by NoSweat
"mach1. " ~
"asi " ~
"mach " ~
"mach.company = gcompany " ~
"m-code " ~
"2 " ~
"19 " ~
"55 " ~
"m-code,m-dscr " ~
"Code,Description " ~
"yes " ~
"m-code,m-dscr " ~
"Machine " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"mach1. " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file mach
&Scoped-define where-statement mach.company = gcompany
&Scoped-define return-field m-code
&Scoped-define font 2
&Scoped-define height-size 19
&Scoped-define width-size 55
&Scoped-define show-fields mach.m-code mach.m-dscr
&Scoped-define show-fields-yellow mach.m-code LABEL-BGCOLOR 14 mach.m-dscr LABEL-BGCOLOR 14
&Scoped-define frame-title Machine
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname mach1.
&Scoped-define window-size 24.7
&Scoped-define window-col 47.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 49
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 48
&Scoped-define btn-cancel-col 41
&Scoped-define auto-find-row 23.6

&Global-define FORMAT-1 x(6)
&Scoped-define FLDNAME1 mach.m-code
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Code
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 mach.m-dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
