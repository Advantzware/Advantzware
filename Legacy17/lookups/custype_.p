/* custype_.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"custype_. " ~
"ASI " ~
"mach " ~
"mach.company = gcompany " ~
"m-code " ~
"3 " ~
"19 " ~
"34 " ~
"m-code,m-dscr " ~
"Code,Description " ~
"yes " ~
"m-code,m-dscr " ~
"Machine# Lookups " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"mach. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file mach
&Scoped-define where-statement mach.company = gcompany
&Scoped-define return-field m-code
&Scoped-define font 3
&Scoped-define height-size 19
&Scoped-define width-size 34
&Scoped-define show-fields mach.m-code mach.m-dscr
&Scoped-define show-fields-yellow mach.m-code LABEL-BGCOLOR 14 mach.m-dscr LABEL-BGCOLOR 14
&Scoped-define frame-title Machine# Lookups
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname mach.
&Scoped-define window-size 24
&Scoped-define window-col 58
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 28
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 27
&Scoped-define btn-cancel-col 20
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(6)
&Scoped-define FLDNAME1 mach.m-code
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Code
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 mach.m-dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
