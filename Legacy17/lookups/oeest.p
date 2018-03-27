/* oeest.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"oeest. " ~
"ASI " ~
"eb " ~
" " ~
"est-no " ~
"4 " ~
"19 " ~
"46 " ~
"est-no,cust-no,est-type " ~
"est-no " ~
"no " ~
"est-no,cust-no,est-type " ~
" " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared} " ~
" " ~
"est. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file eb
&Scoped-define where-statement TRUE
&Scoped-define return-field est-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields eb.est-no eb.cust-no eb.est-type
&Scoped-define show-fields-yellow eb.est-no LABEL-BGCOLOR 14 eb.cust-no LABEL-BGCOLOR 14 eb.est-type LABEL-BGCOLOR 14
&Scoped-define frame-title 
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname est.
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

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 eb.est-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 est-no

{methods/lookup.i}
