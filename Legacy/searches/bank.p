/* bank.p - Generated 06/20/2002 - 10:40 am by nosweat
"bank. " ~
"ASI " ~
"bank " ~
"bank.company = gcompany " ~
"bank-code " ~
"4 " ~
"17 " ~
"36 " ~
"bank-code,bank-name " ~
"Bank,Name " ~
"no " ~
"bank-code,bank-name " ~
"bank-name " ~
"Banks Search " ~
"{custom/getcmpny.i} " ~
"{custom/gcompany.i} " ~
" " ~
"actnum. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file bank
&Scoped-define where-statement bank.company = gcompany
&Scoped-define return-field bank-code
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 36
&Scoped-define show-fields bank.bank-code bank.bank-name
&Scoped-define frame-title Banks Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 16
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname actnum.
&Scoped-define window-size 23
&Scoped-define window-col 57
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 30
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 23
&Scoped-define btn-cancel-col 12
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(8)
&Scoped-define FLDNAME1 bank.bank-code
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Bank
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 bank.bank-name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Name
&Scoped-define WORDFLD bank-name

{methods/search.i}
