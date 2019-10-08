/* mchargc.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"mchargc. " ~
"asi " ~
"job-code " ~
" " ~
"code " ~
"4 " ~
"19 " ~
"46 " ~
"code,dscr " ~
"Code,Description " ~
"yes " ~
"code,dscr " ~
"Machine Charge Codes Lookup " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"job-code. " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file job-code
&Scoped-define where-statement TRUE
&Scoped-define return-field code
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields job-code.code job-code.dscr
&Scoped-define show-fields-yellow job-code.code LABEL-BGCOLOR 14 job-code.dscr LABEL-BGCOLOR 14
&Scoped-define frame-title Machine Charge Codes Lookup
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname job-code.
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

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 job-code.code
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Code
&Global-define FORMAT-2 x(45)
&Scoped-define FLDNAME2 job-code.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
