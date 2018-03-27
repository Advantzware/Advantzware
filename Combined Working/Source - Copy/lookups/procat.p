/* procat.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"procat. " ~
"ASI " ~
"procat " ~
"procat.company = g_company " ~
"procat " ~
"2 " ~
"19 " ~
"45 " ~
"procat,dscr " ~
"Category,Description " ~
"yes " ~
"procat,dscr " ~
"Product Categories Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"procat. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file procat
&Scoped-define where-statement procat.company = g_company
&Scoped-define return-field procat
&Scoped-define font 2
&Scoped-define height-size 19
&Scoped-define width-size 45
&Scoped-define show-fields procat.procat procat.dscr
&Scoped-define show-fields-yellow procat.procat LABEL-BGCOLOR 14 procat.dscr LABEL-BGCOLOR 14
&Scoped-define frame-title Product Categories Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname procat.
&Scoped-define window-size 24
&Scoped-define window-col 52.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 39
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 38
&Scoped-define btn-cancel-col 31
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 procat.procat
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Category
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 procat.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
