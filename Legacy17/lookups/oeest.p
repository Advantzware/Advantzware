/* oeest.p - Generated 10/10/2003 - 10:17 am by nosweat
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
&Scoped-define where-statement eb.company eq cocode
&Scoped-define return-field est-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields eb.est-no eb.cust-no eb.est-type
&Scoped-define frame-title 
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname est.
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

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 eb.est-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 est-no

{methods/lookup.i}
