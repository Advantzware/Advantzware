/* i-nop.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"i-nop. " ~
"ASI " ~
"itemfg " ~
" " ~
"i-no " ~
"4 " ~
"19 " ~
"80 " ~
"i-no,i-name,cust-name " ~
"Item No,Name,Customer Name " ~
"yes " ~
"i-no,i-name,cust-name " ~
" " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/
DEF VAR useColors AS cha NO-UNDO.
&Scoped-define lookup-db ASI.
&Scoped-define lookup-file itemfg
&Scoped-define where-statement TRUE
&Scoped-define return-field i-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 80
&Scoped-define show-fields itemfg.i-no itemfg.i-name itemfg.cust-name
&Scoped-define show-fields-yellow itemfg.i-no LABEL-BGCOLOR 14 itemfg.i-name LABEL-BGCOLOR 14 itemfg.cust-name LABEL-BGCOLOR 14
&Scoped-define frame-title 
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 24
&Scoped-define window-col 35
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 74
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 73
&Scoped-define btn-cancel-col 66
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(15)
&Scoped-define FLDNAME1 itemfg.i-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Item No
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 itemfg.i-name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Name
&Global-define FORMAT-3 x(30)
&Scoped-define FLDNAME3 itemfg.cust-name
&Scoped-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-2}
&Scoped-define DESCRIP3 Customer Name

{methods/lookup.i}
