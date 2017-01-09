/* mach.p - Generated 08/24/2000 - 11:41 am by nosweat
"mach. " ~
"ASI " ~
"mach " ~
" " ~
"m-code " ~
"2 " ~
"19 " ~
"46 " ~
"m-code,m-dscr " ~
"Code,Description " ~
"yes " ~
"m-code,m-dscr " ~
"Machine Code " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"mach. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file mach
&Scoped-define where-statement mach.company eq cocode
&Scoped-define return-field m-code
&Scoped-define font 2
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields mach.m-code mach.m-dscr
&Scoped-define frame-title Machine Code
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname mach.
&Scoped-define window-size 23
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 37
&Scoped-define btn-cancel-col 26
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(6)
&Scoped-define FLDNAME1 mach.m-code
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Code
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 mach.m-dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
