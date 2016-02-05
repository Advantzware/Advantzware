/* costtype.p - Generated 08/28/2001 -  4:09 pm by nosweat
"costtype. " ~
"ASI " ~
"costtype " ~
"costtype.company = gcompany AND costtype.loc = gloc " ~
"cost-type " ~
"4 " ~
"19 " ~
"120 " ~
"cost-type,descr,inv-asset,pur-var,cons-exp " ~
"Cost Type,Description " ~
"yes " ~
"cost-type,descr " ~
"Material Cost Types Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared} " ~
" " ~
"costtype. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file costtype
&Scoped-define where-statement costtype.company = gcompany AND costtype.loc = gloc
&Scoped-define return-field cost-type
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 120
&Scoped-define show-fields costtype.cost-type costtype.descr costtype.inv-asset costtype.pur-var costtype.cons-exp
&Scoped-define frame-title Material Cost Types Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname costtype.
&Scoped-define window-size 23
&Scoped-define window-col 15
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 114
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 111
&Scoped-define btn-cancel-col 100
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 X(3)
&Scoped-define FLDNAME1 costtype.cost-type
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Cost Type
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 costtype.descr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
