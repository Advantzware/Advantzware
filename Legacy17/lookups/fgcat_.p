/* fgcat_.p - Generated 01/19/2000 -  1:20 pm by nosweat
"fgcat_. " ~
"ASI " ~
"fgcat " ~
"fgcat.company = gcompany " ~
"dscr " ~
"4 " ~
"19 " ~
"38 " ~
"dscr,procat " ~
"Description,Category " ~
"yes " ~
"dscr,procat " ~
"Finished Goods Categories Description Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"fgcat. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file fgcat
&Scoped-define where-statement fgcat.company = gcompany
&Scoped-define return-field dscr
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 38
&Scoped-define show-fields fgcat.dscr fgcat.procat
&Scoped-define frame-title Finished Goods Categories Description Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname fgcat.
&Scoped-define window-size 23
&Scoped-define window-col 56
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 32
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 29
&Scoped-define btn-cancel-col 18
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(20)
&Scoped-define FLDNAME1 fgcat.dscr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 x(5)
&Scoped-define FLDNAME2 fgcat.procat
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Category

{methods/lookup.i}
