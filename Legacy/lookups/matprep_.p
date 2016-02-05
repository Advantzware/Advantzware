/* matprep_.p - Generated 01/18/2000 - 10:06 am by nosweat
"matprep_. " ~
"ASI " ~
"matprep " ~
"matprep.company = gcompany " ~
"dscr " ~
"4 " ~
"19 " ~
"41 " ~
"dscr,mat " ~
"Description,Type " ~
"yes " ~
"dscr,mat " ~
"Preparation Material Codes Description Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"matprep. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file matprep
&Scoped-define where-statement matprep.company = gcompany
&Scoped-define return-field dscr
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 41
&Scoped-define show-fields matprep.dscr matprep.mat
&Scoped-define frame-title Preparation Material Codes Description Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname matprep.
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
&Scoped-define FLDNAME1 matprep.dscr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 x(5)
&Scoped-define FLDNAME2 matprep.mat
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Type

{methods/lookup.i}
