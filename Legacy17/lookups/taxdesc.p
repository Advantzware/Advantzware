/* taxdesc.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"taxdesc. " ~
"asi " ~
"stax " ~
" " ~
"tax-code[1] " ~
"4 " ~
"19 " ~
"46 " ~
"tax-code[1],tax-dscr[1] " ~
" " ~
"yes " ~
"tax-code[1],tax-dscr[1] " ~
"Tax Codes Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"taxdesc. " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file stax
&Scoped-define where-statement stax.company eq cocode
&Scoped-define return-field tax-code1[1]
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields stax.tax-code[1] stax.tax-dscr[1]
&Scoped-define show-fields-yellow stax.tax-code[1] LABEL-BGCOLOR 14 stax.tax-dscr[1] LABEL-BGCOLOR 14
&Scoped-define frame-title Tax Codes Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname taxdesc.
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


{methods/lookup.i}
