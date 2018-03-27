/* poprint.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"poprint. " ~
"ASI " ~
"po-ord " ~
" " ~
"po-no " ~
"3 " ~
"19 " ~
"66 " ~
"po-no,po-date,vend-no,printed,stat " ~
"PO Number,Vendor " ~
"yes " ~
"po-no,vend-no " ~
" " ~
"{custom/yellowColumns.i} ~{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/poprint.i} ~{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"poprint. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file po-ord
&Scoped-define where-statement TRUE
&Scoped-define return-field po-no
&Scoped-define font 3
&Scoped-define height-size 19
&Scoped-define width-size 66
&Scoped-define show-fields po-ord.po-no po-ord.po-date po-ord.vend-no po-ord.printed po-ord.stat
&Scoped-define show-fields-yellow po-ord.po-no LABEL-BGCOLOR 14 po-ord.po-date LABEL-BGCOLOR 14 po-ord.vend-no LABEL-BGCOLOR 14 po-ord.printed LABEL-BGCOLOR 14 po-ord.stat LABEL-BGCOLOR 14
&Scoped-define frame-title 
&Scoped-define top-include ~{custom/yellowColumns.i} ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/poprint.i} ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname poprint.
&Scoped-define window-size 24
&Scoped-define window-col 42
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 60
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 59
&Scoped-define btn-cancel-col 52
&Scoped-define auto-find-row 23.65

&Global-define DATATYP1 INTEGER
&Global-define FORMAT-1 >>>>>9
&Scoped-define FLDNAME1 po-ord.po-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 PO Number
&Global-define FORMAT-2 x(8)
&Scoped-define FLDNAME2 po-ord.vend-no
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Vendor

{methods/lookup.i}
