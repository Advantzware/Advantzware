/* fgcat.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"fgcat. " ~
"ASI " ~
"fgcat " ~
"fgcat.company = gcompany " ~
"procat " ~
"4 " ~
"19 " ~
"38 " ~
"procat,dscr " ~
"Category,Description " ~
"yes " ~
"procat,dscr " ~
"Finished Goods Categories Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"fgcat. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file fgcat
&Scoped-define where-statement fgcat.company = gcompany
&Scoped-define return-field procat
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 38
&Scoped-define show-fields fgcat.procat fgcat.dscr
&Scoped-define show-fields-yellow fgcat.procat LABEL-BGCOLOR 14 fgcat.dscr LABEL-BGCOLOR 14
&Scoped-define frame-title Finished Goods Categories Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname fgcat.
&Scoped-define window-size 24
&Scoped-define window-col 56
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 32
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 31
&Scoped-define btn-cancel-col 24
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 fgcat.procat
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Category
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 fgcat.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
