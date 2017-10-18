/* prod_.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"prod_. " ~
"ASI " ~
"prod " ~
"prod.company = gcompany " ~
"dscr " ~
"4 " ~
"19 " ~
"63 " ~
"dscr,prolin " ~
"Description,Product Line " ~
"no " ~
"dscr,prolin " ~
"Product Line GL Accounts Description Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"prod. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file prod
&Scoped-define where-statement prod.company = gcompany
&Scoped-define return-field dscr
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 63
&Scoped-define show-fields prod.dscr prod.prolin
&Scoped-define show-fields-yellow prod.dscr LABEL-BGCOLOR 14 prod.prolin LABEL-BGCOLOR 14
&Scoped-define frame-title Product Line GL Accounts Description Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname prod.
&Scoped-define window-size 24
&Scoped-define window-col 43.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 57
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 56
&Scoped-define btn-cancel-col 49
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(45)
&Scoped-define FLDNAME1 prod.dscr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 x(8)
&Scoped-define FLDNAME2 prod.prolin
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Product Line

{methods/lookup.i}
