/* cust.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"cust. " ~
"ASI " ~
"cust " ~
"cust.company EQ gcompany " ~
"cust-no " ~
"3 " ~
"19 " ~
"150 " ~
"cust-no,name,city,state,zip,type,sman,terr " ~
"Cust. #,Customer Name " ~
"yes " ~
"cust-no,name " ~
"Customers Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
"cust. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file cust
&Scoped-define where-statement cust.company EQ gcompany
&Scoped-define return-field cust-no
&Scoped-define font 3
&Scoped-define height-size 19
&Scoped-define width-size 150
&Scoped-define show-fields cust.cust-no cust.name cust.city cust.state cust.zip cust.type cust.sman cust.terr
&Scoped-define show-fields-yellow cust.cust-no LABEL-BGCOLOR 14 cust.name LABEL-BGCOLOR 14 cust.city LABEL-BGCOLOR 14 cust.state LABEL-BGCOLOR 14 cust.zip LABEL-BGCOLOR 14 cust.type LABEL-BGCOLOR 14 cust.sman LABEL-BGCOLOR 14 cust.terr LABEL-BGCOLOR 14
&Scoped-define frame-title Customers Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname cust.
&Scoped-define window-size 24
&Scoped-define window-col 0
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 144
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 143
&Scoped-define btn-cancel-col 136
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(8)
&Scoped-define FLDNAME1 cust.cust-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Cust. #
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 cust.name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Customer Name

{methods/lookup.i}
