/* stax.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"stax. " ~
"ASI " ~
"stax " ~
"stax.company = gcompany " ~
"tax-group " ~
"2 " ~
"19 " ~
"95 " ~
"tax-group,tax-code[1],tax-code[2],tax-code[3],tax-dscr[1] " ~
"Sales Tax Group " ~
"yes " ~
"tax-group,tax-dscr[1] " ~
"Tax Codes Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"stax. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file stax
&Scoped-define where-statement stax.company = gcompany
&Scoped-define return-field tax-group
&Scoped-define font 2
&Scoped-define height-size 19
&Scoped-define width-size 120
&Scoped-define show-fields stax.tax-group stax.tax-code1[1] stax.tax-dscr1[1] stax.tax-code1[2] stax.tax-code1[3] stax.tax-code1[4]stax.tax-dscr1[4]stax.tax-code1[5]stax.tax-dscr1[5]
&Scoped-define show-fields-yellow stax.tax-group LABEL-BGCOLOR 14 stax.tax-code[1] LABEL-BGCOLOR 14 stax.tax-code[2] LABEL-BGCOLOR 14 stax.tax-code[3] LABEL-BGCOLOR 14 stax.tax-dscr[1] LABEL-BGCOLOR 14
&Scoped-define frame-title Tax Codes Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname stax.
&Scoped-define window-size 24
&Scoped-define window-col 27.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 89
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 88
&Scoped-define btn-cancel-col 81
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(3)
&Scoped-define FLDNAME1 stax.tax-group
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Sales Tax Group

{methods/lookup.i}
