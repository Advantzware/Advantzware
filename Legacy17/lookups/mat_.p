/* mat_.p - Generated 12/24/2002 -  1:22 pm by nosweat
"mat_. " ~
"ASI " ~
"mat " ~
" " ~
"dscr " ~
"4 " ~
"19 " ~
"41 " ~
"dscr,mat " ~
"Description,Type " ~
"yes " ~
"dscr,mat " ~
"Material Types Description Lookup " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"mat. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file mat
&Scoped-define where-statement TRUE
&Scoped-define return-field dscr
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 41
&Scoped-define show-fields mat.dscr mat.mat
&Scoped-define frame-title Material Types Description Lookup
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname mat.
&Scoped-define window-size 23
&Scoped-define window-col 54.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 35
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 32
&Scoped-define btn-cancel-col 21
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(30)
&Scoped-define FLDNAME1 mat.dscr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 x(5)
&Scoped-define FLDNAME2 mat.mat
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Type

{methods/lookup.i}
