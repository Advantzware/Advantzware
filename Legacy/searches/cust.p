/* cust.p - Generated 01/24/2000 -  2:44 pm by nosweat
"cust. " ~
"ASI " ~
"cust " ~
"cust.company = gcompany " ~
"cust-no " ~
"4 " ~
"17 " ~
"44 " ~
"cust-no,name " ~
"Cust. #,Customer Name " ~
"yes " ~
"cust-no,name " ~
"name " ~
"Customers Search " ~
"{custom/getcmpny.i} " ~
"{custom/gcompany.i} " ~
" " ~
"cust. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file cust
&Scoped-define where-statement cust.company = gcompany
&Scoped-define return-field cust-no
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 44
&Scoped-define show-fields cust.cust-no cust.name
&Scoped-define frame-title Customers Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 24
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname cust.
&Scoped-define window-size 23
&Scoped-define window-col 53
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 38
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 31
&Scoped-define btn-cancel-col 20
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(8)
&Scoped-define FLDNAME1 cust.cust-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Cust. #
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 cust.name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Customer Name
&Scoped-define WORDFLD name

{methods/search.i}
