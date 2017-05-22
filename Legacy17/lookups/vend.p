/* vend.p - Generated 07/13/2004 -  9:59 am by nosweat
"vend. " ~
"ASI " ~
"vend " ~
"vend.company = gcompany " ~
"vend-no " ~
"3 " ~
"19 " ~
"55 " ~
"vend-no,name " ~
"Name,Vend.# " ~
"yes " ~
"name,vend-no " ~
"Vendor Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"vend. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file vend
&Scoped-define where-statement vend.company = gcompany
&Scoped-define return-field vend-no
&Scoped-define font 3
&Scoped-define height-size 19
&Scoped-define width-size 55
&Scoped-define show-fields vend.vend-no vend.name
&Scoped-define frame-title Vendor Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname vend.
&Scoped-define window-size 23
&Scoped-define window-col 47.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 49
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 46
&Scoped-define btn-cancel-col 35
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(30)
&Scoped-define FLDNAME1 vend.name
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Name
&Global-define FORMAT-2 x(8)
&Scoped-define FLDNAME2 vend.vend-no
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Vend.#
&SCOPED-DEFINE useMatches YES    /*Task# 11181313*/

{methods/lookup.i}
