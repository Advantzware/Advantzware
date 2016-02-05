/* currency.p - Generated 07/16/2002 -  9:13 am by nosweat
"currency. " ~
"ASI " ~
"currency " ~
" " ~
"c-code " ~
"4 " ~
"19 " ~
"46 " ~
"c-code,c-desc,ex-rate " ~
"Code,description " ~
"yes " ~
"c-code,c-desc " ~
"Currency Code " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"currency. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file currency
&Scoped-define where-statement currency.company = gcompany
&Scoped-define return-field c-code
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields currency.c-code currency.c-desc currency.ex-rate
&Scoped-define frame-title Currency Code
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname currency.
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

&Global-define FORMAT-1 X(3)
&Scoped-define FLDNAME1 currency.c-code
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Code
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 currency.c-desc
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 description

{methods/lookup.i}
