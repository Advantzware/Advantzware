/* cust.p - Generated 01/20/2006 -  4:23 pm by nosweat
"cust. " ~
"ASI " ~
"cust " ~
"cust.company = gcompany " ~
"cust-no " ~
"3 " ~
"19 " ~
"120 " ~
"cust-no,name,city,state,zip,type,sman,terr " ~
"Customer Name,Cust. # " ~
"yes " ~
"name,cust-no " ~
"Customers Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
"cust. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file cust
&Scoped-define where-statement cust.company = gcompany
&Scoped-define return-field cust-no
&Scoped-define font 3
&Scoped-define height-size 19
&Scoped-define width-size 120
&Scoped-define show-fields cust.cust-no cust.name cust.city cust.state cust.zip cust.type cust.sman cust.terr
&Scoped-define frame-title Customers Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname cust.
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

&Global-define FORMAT-1 x(30)
&Scoped-define FLDNAME1 cust.name
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Customer Name
&Global-define FORMAT-2 x(8)
&Scoped-define FLDNAME2 cust.cust-no
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Cust. #

{methods/lookup.i}
