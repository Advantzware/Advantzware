/* taxcode.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"taxcode. " ~
"asi " ~
"stax " ~
" " ~
"tax-group " ~
"4 " ~
"19 " ~
"46 " ~
"tax-group " ~
"Sales Tax Group " ~
"yes " ~
"tax-group " ~
"Tax Codes Lookup " ~
"~{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"~{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"stax. " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file stax
&Scoped-define where-statement stax.company eq cocode
&Scoped-define return-field tax-group
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields stax.tax-group
&Scoped-define show-fields-yellow stax.tax-group LABEL-BGCOLOR 14
&Scoped-define frame-title Tax Codes Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname stax.
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
&Scoped-define FLDNAME1 stax.tax-group
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Sales Tax Group

{methods/lookup.i}
