/* taxgrp.p - Generated 08/07/2003 -  4:31 pm by nosweat
"taxgrp. " ~
"asi " ~
"stax-group " ~
" " ~
"tax-group " ~
"4 " ~
"19 " ~
"46 " ~
"tax-group,tax-dscr " ~
"Tax Group,Description " ~
"yes " ~
"tax-group,tax-dscr " ~
"Tax Groups " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file stax-group
&Scoped-define where-statement stax-group.company eq cocode
&Scoped-define return-field tax-group
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields stax-group.tax-group stax-group.tax-dscr
&Scoped-define frame-title Tax Groups
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
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

&Global-define FORMAT-1 x(3)
&Scoped-define FLDNAME1 stax-group.tax-group
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Tax Group
&Global-define FORMAT-2 x(8)
&Scoped-define FLDNAME2 stax-group.tax-dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
