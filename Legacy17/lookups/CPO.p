/* CPO.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"CPO. " ~
"ASI " ~
"oe-rel " ~
" " ~
"po-no " ~
"4 " ~
"19 " ~
"46 " ~
"po-no,cust-no,rel-date " ~
"P.O. Num " ~
"yes " ~
"po-no " ~
"PO Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared} " ~
" " ~
"CPO. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file oe-rel
&Scoped-define where-statement TRUE
&Scoped-define return-field po-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields oe-rel.po-no oe-rel.cust-no oe-rel.rel-date
&Scoped-define show-fields-yellow oe-rel.po-no LABEL-BGCOLOR 14 oe-rel.cust-no LABEL-BGCOLOR 14 oe-rel.rel-date LABEL-BGCOLOR 14
&Scoped-define frame-title PO Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname CPO.
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

&Global-define FORMAT-1 x(15)
&Scoped-define FLDNAME1 oe-rel.po-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 P.O. Num

{methods/lookup.i}
