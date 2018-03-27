/* flute.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"flute. " ~
"asi " ~
"flute " ~
"flute.company=gcompany " ~
"code " ~
"4 " ~
"19 " ~
"46 " ~
"code,dscr " ~
"Code,Description " ~
"yes " ~
"code,dscr " ~
" " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"flute. " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file flute
&Scoped-define where-statement flute.company=gcompany
&Scoped-define return-field code
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields flute.code flute.dscr
&Scoped-define show-fields-yellow flute.code LABEL-BGCOLOR 14 flute.dscr LABEL-BGCOLOR 14
&Scoped-define frame-title 
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname flute.
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

&Global-define FORMAT-1 x(3)
&Scoped-define FLDNAME1 flute.code
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Code
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 flute.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
