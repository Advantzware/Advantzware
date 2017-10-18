/* loc-bin.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"loc-bin. " ~
"asi " ~
"fg-bin " ~
"fg-bin.company = gcompany and fg-bin.i-no =  '' " ~
"loc-bin " ~
"4 " ~
"19 " ~
"46 " ~
"loc,loc-bin " ~
"Warehouse,Primary Bin Loc. " ~
"yes " ~
"loc,loc-bin " ~
"Bins Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"loc-bin. " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file fg-bin
&Scoped-define where-statement fg-bin.company = gcompany and fg-bin.i-no =  ''
&Scoped-define return-field loc-bin
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields fg-bin.loc fg-bin.loc-bin
&Scoped-define show-fields-yellow fg-bin.loc LABEL-BGCOLOR 14 fg-bin.loc-bin LABEL-BGCOLOR 14
&Scoped-define frame-title Bins Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname loc-bin.
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
&Scoped-define FLDNAME1 fg-bin.loc
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Warehouse
&Global-define FORMAT-2 x(8)
&Scoped-define FLDNAME2 fg-bin.loc-bin
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Primary Bin Loc.

{methods/lookup.i}
