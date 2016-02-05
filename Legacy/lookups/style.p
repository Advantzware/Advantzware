/* style.p - Generated 06/28/2005 -  3:48 pm by nosweat
"style. " ~
"ASI " ~
"style " ~
"style.company = gcompany " ~
"style " ~
"4 " ~
"19 " ~
"46 " ~
"style,dscr " ~
"Style No.,Description " ~
"yes " ~
"style,dscr " ~
" " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"style. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file style
&Scoped-define where-statement style.company = gcompany
&Scoped-define return-field style
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields style.style style.dscr
&Scoped-define frame-title 
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname style.
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
&Scoped-define FLDNAME1 style.style
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Style No.
&Global-define FORMAT-2 x(25)
&Scoped-define FLDNAME2 style.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
