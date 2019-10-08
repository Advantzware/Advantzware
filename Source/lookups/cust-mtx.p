/* cust-mtx.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"cust-mtx. " ~
"ASI " ~
"carr-mtx " ~
"carr-mtx.company = gcompany AND carr-mtx.loc = s-loc AND carr-mtx.carrier = s-carrier " ~
"del-zone " ~
"2 " ~
"19 " ~
"67 " ~
"del-zone,del-dscr,del-zip " ~
"Zone,Description " ~
"yes " ~
"del-zone,del-dscr " ~
"Delivery Zone Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{methods/defines/shipto.i} ~{sys/inc/var.i new shared} " ~
" " ~
"carr-mtx. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file carr-mtx
&Scoped-define where-statement carr-mtx.company = gcompany AND carr-mtx.loc = s-loc AND carr-mtx.carrier = s-carrier
&Scoped-define return-field del-zone
&Scoped-define font 2
&Scoped-define height-size 19
&Scoped-define width-size 67
&Scoped-define show-fields carr-mtx.del-zone carr-mtx.del-dscr carr-mtx.del-zip
&Scoped-define show-fields-yellow carr-mtx.del-zone LABEL-BGCOLOR 14 carr-mtx.del-dscr LABEL-BGCOLOR 14 carr-mtx.del-zip LABEL-BGCOLOR 14
&Scoped-define frame-title Delivery Zone Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{methods/defines/shipto.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname carr-mtx.
&Scoped-define window-size 24
&Scoped-define window-col 41.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 61
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 60
&Scoped-define btn-cancel-col 53
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 carr-mtx.del-zone
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Zone
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 carr-mtx.del-dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
