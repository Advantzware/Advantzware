/* bank_.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"bank_. " ~
"ASI " ~
"bank " ~
"bank.company = gcompany " ~
"bank-name " ~
"4 " ~
"19 " ~
"36 " ~
"bank-name,bank-code " ~
"Name,Bank " ~
"no " ~
"bank-name,bank-code " ~
"Banks Name Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"bank. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file bank
&Scoped-define where-statement bank.company = gcompany
&Scoped-define return-field bank-name
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 36
&Scoped-define show-fields bank.bank-name bank.bank-code
&Scoped-define show-fields-yellow bank.bank-name LABEL-BGCOLOR 14 bank.bank-code LABEL-BGCOLOR 14
&Scoped-define frame-title Banks Name Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname bank.
&Scoped-define window-size 24
&Scoped-define window-col 57
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 30
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 29
&Scoped-define btn-cancel-col 22
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(20)
&Scoped-define FLDNAME1 bank.bank-name
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Name
&Global-define FORMAT-2 x(8)
&Scoped-define FLDNAME2 bank.bank-code
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Bank

{methods/lookup.i}
