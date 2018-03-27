/* oepo.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"oepo. " ~
"asi " ~
"oe-rel " ~
"oe-rel.po-no <> '' " ~
"po-no " ~
"2 " ~
"19 " ~
"45 " ~
"po-no,cust-no " ~
"P.O. Num,Cust. # " ~
"yes " ~
"po-no,cust-no " ~
"Order PO# " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"oepo. " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file oe-rel
&Scoped-define where-statement oe-rel.po-no <> ''
&Scoped-define return-field po-no
&Scoped-define font 2
&Scoped-define height-size 19
&Scoped-define width-size 45
&Scoped-define show-fields oe-rel.po-no oe-rel.cust-no
&Scoped-define show-fields-yellow oe-rel.po-no LABEL-BGCOLOR 14 oe-rel.cust-no LABEL-BGCOLOR 14
&Scoped-define frame-title Order PO#
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname oepo.
&Scoped-define window-size 24
&Scoped-define window-col 52.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 39
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 38
&Scoped-define btn-cancel-col 31
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(15)
&Scoped-define FLDNAME1 oe-rel.po-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 P.O. Num
&Global-define FORMAT-2 x(8)
&Scoped-define FLDNAME2 oe-rel.cust-no
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Cust. #

{methods/lookup.i}
