/* rejct-c_.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"rejct-c_. " ~
"ASI " ~
"rejct-cd " ~
" " ~
"dscr " ~
"4 " ~
"19 " ~
"56 " ~
"dscr,code " ~
"Description,Code " ~
"yes " ~
"dscr,code " ~
"PO Rejection Codes Description Lookup " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"rejct-cd. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file rejct-cd
&Scoped-define where-statement TRUE
&Scoped-define return-field dscr
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 56
&Scoped-define show-fields rejct-cd.dscr rejct-cd.code
&Scoped-define show-fields-yellow rejct-cd.dscr LABEL-BGCOLOR 14 rejct-cd.code LABEL-BGCOLOR 14
&Scoped-define frame-title PO Rejection Codes Description Lookup
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname rejct-cd.
&Scoped-define window-size 24
&Scoped-define window-col 47
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 50
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 49
&Scoped-define btn-cancel-col 42
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(45)
&Scoped-define FLDNAME1 rejct-cd.dscr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 x(2)
&Scoped-define FLDNAME2 rejct-cd.code
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Code

{methods/lookup.i}
