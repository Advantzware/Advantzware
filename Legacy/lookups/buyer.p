/* buyer.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"buyer. " ~
"ASI " ~
"buyer " ~
"buyer.company = gcompany " ~
"buyer " ~
"4 " ~
"19 " ~
"34 " ~
"buyer,buyer-n " ~
"Buyer,Buyer's Name " ~
"yes " ~
"buyer,buyer-n " ~
"Buyers Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"buyer. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file buyer
&Scoped-define where-statement buyer.company = gcompany
&Scoped-define return-field buyer
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 34
&Scoped-define show-fields buyer.buyer buyer.buyer-n
&Scoped-define show-fields-yellow buyer.buyer LABEL-BGCOLOR 14 buyer.buyer-n LABEL-BGCOLOR 14
&Scoped-define frame-title Buyers Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname buyer.
&Scoped-define window-size 24
&Scoped-define window-col 58
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 28
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 27
&Scoped-define btn-cancel-col 20
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(3)
&Scoped-define FLDNAME1 buyer.buyer
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Buyer
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 buyer.buyer-n
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Buyer's Name

{methods/lookup.i}
