/* taxgrp_.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"taxgrp_. " ~
"asi " ~
"stax-group " ~
" " ~
"tax-dscr " ~
"4 " ~
"19 " ~
"46 " ~
"tax-dscr,tax-group " ~
"Description,Tax Group " ~
"yes " ~
"tax-dscr,tax-group " ~
"Tax Groups " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file stax-group
&Scoped-define where-statement stax-group.company eq cocode
&Scoped-define return-field tax-dscr
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields stax-group.tax-dscr stax-group.tax-group
&Scoped-define show-fields-yellow stax-group.tax-dscr LABEL-BGCOLOR 14 stax-group.tax-group LABEL-BGCOLOR 14
&Scoped-define frame-title Tax Groups
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
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

&Global-define FORMAT-1 x(8)
&Scoped-define FLDNAME1 stax-group.tax-dscr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 x(3)
&Scoped-define FLDNAME2 stax-group.tax-group
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Tax Group

{methods/lookup.i}
