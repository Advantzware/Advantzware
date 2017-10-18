/* stax-grp.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"stax-grp. " ~
"ASI " ~
"stax-group " ~
"stax.company eq g_company and stax.tax-group eq stax.tax-code[1] " ~
"tax-group " ~
"4 " ~
"19 " ~
"46 " ~
"tax-group,tax-dscr " ~
"Tax Group " ~
"yes " ~
"tax-group " ~
"Tax Groups " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file stax-group
&Scoped-define where-statement stax.company eq g_company and stax.tax-group eq stax.tax-code[1]
&Scoped-define return-field tax-group
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields stax-group.tax-group stax-group.tax-dscr
&Scoped-define show-fields-yellow stax-group.tax-group LABEL-BGCOLOR 14 stax-group.tax-dscr LABEL-BGCOLOR 14
&Scoped-define frame-title Tax Groups
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
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

&Global-define FORMAT-1 x(3)
&Scoped-define FLDNAME1 stax-group.tax-group
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Tax Group

{methods/lookup.i}
