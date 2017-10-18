/* order.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"order. " ~
"ASI " ~
"oe-ord " ~
"oe-ord.company = gcompany " ~
"ord-no " ~
"3 " ~
"19 " ~
"71 " ~
"ord-no,est-no,cust-name " ~
"Order#,Estimate #,Customer Name " ~
"yes " ~
"ord-no,est-no,cust-name " ~
" " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i}~{sys/inc/var.i new shared} " ~
" " ~
"order. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file oe-ord
&Scoped-define where-statement oe-ord.company = gcompany
&Scoped-define return-field ord-no
&Scoped-define font 3
&Scoped-define height-size 19
&Scoped-define width-size 71
&Scoped-define show-fields oe-ord.ord-no oe-ord.est-no oe-ord.cust-name
&Scoped-define show-fields-yellow oe-ord.ord-no LABEL-BGCOLOR 14 oe-ord.est-no LABEL-BGCOLOR 14 oe-ord.cust-name LABEL-BGCOLOR 14
&Scoped-define frame-title 
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i}~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname order.
&Scoped-define window-size 24
&Scoped-define window-col 39.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 65
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 64
&Scoped-define btn-cancel-col 57
&Scoped-define auto-find-row 23.65

&Global-define DATATYP1 INTEGER
&Global-define FORMAT-1 >>>>>9
&Scoped-define FLDNAME1 oe-ord.ord-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Order#
&Global-define FORMAT-2 x(5)
&Scoped-define FLDNAME2 oe-ord.est-no
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Estimate #
&Global-define FORMAT-3 x(30)
&Scoped-define FLDNAME3 oe-ord.cust-name
&Scoped-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-2}
&Scoped-define DESCRIP3 Customer Name

{methods/lookup.i}
