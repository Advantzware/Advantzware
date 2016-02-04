/* invoc.p - Generated 03/08/2005 - 10:33 am by nosweat
"invoc. " ~
"ASI " ~
"inv-head " ~
"inv-head.company eq cocode and inv-head.posted eq no " ~
"inv-no " ~
"3 " ~
"19 " ~
"60 " ~
"inv-no,bill-to,printed,bol-no " ~
"Invoice Number,Bill Of Lading Number " ~
"yes " ~
"inv-no,bol-no " ~
" " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"invoc. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file inv-head
&Scoped-define where-statement inv-head.company eq cocode and inv-head.posted eq no
&Scoped-define return-field inv-no
&Scoped-define font 3
&Scoped-define height-size 19
&Scoped-define width-size 60
&Scoped-define show-fields inv-head.inv-no inv-head.bill-to inv-head.printed inv-head.bol-no
&Scoped-define frame-title 
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname invoc.
&Scoped-define window-size 23
&Scoped-define window-col 45
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 54
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 51
&Scoped-define btn-cancel-col 40
&Scoped-define auto-find-row 22.85

&Global-define DATATYP1 INTEGER
&Global-define FORMAT-1 >>>>>9
&Scoped-define FLDNAME1 inv-head.inv-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Invoice Number
&Global-define DATATYP2 INTEGER
&Global-define FORMAT-2 >>>>>>>9
&Scoped-define FLDNAME2 inv-head.bol-no
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Bill Of Lading Number

{methods/lookup.i}
