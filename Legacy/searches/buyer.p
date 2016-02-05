/* buyer.p - Generated 01/18/2000 -  4:21 pm by nosweat
"buyer. " ~
"ASI " ~
"buyer " ~
"buyer.company = gcompany " ~
"buyer " ~
"4 " ~
"17 " ~
"34 " ~
"buyer,buyer-n " ~
"Buyer,Buyer's Name " ~
"yes " ~
"buyer,buyer-n " ~
"buyer-n " ~
"Buyers Search " ~
"{custom/getcmpny.i} " ~
"{custom/gcompany.i} " ~
" " ~
"buyer. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file buyer
&Scoped-define where-statement buyer.company = gcompany
&Scoped-define return-field buyer
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 34
&Scoped-define show-fields buyer.buyer buyer.buyer-n
&Scoped-define frame-title Buyers Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 14
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname buyer.
&Scoped-define window-size 23
&Scoped-define window-col 58
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 28
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 21
&Scoped-define btn-cancel-col 10
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(3)
&Scoped-define FLDNAME1 buyer.buyer
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Buyer
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 buyer.buyer-n
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Buyer's Name
&Scoped-define WORDFLD buyer-n

{methods/search.i}
