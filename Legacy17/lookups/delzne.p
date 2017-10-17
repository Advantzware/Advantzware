/* delzne.p - Generated 01/28/2003 -  5:03 pm by nosweat
"delzne. " ~
"ASI " ~
"carr-mtx " ~
" " ~
"del-zone " ~
"3 " ~
"19 " ~
"46 " ~
"del-zone,carrier,del-dscr,del-zip " ~
"Zone,Carrier " ~
"yes " ~
"del-zone,carrier " ~
"Carrier Zone Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"carr-mtx. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file carr-mtx
&Scoped-define where-statement carr-mtx.company eq cocode
&Scoped-define return-field del-zone
&Scoped-define font 3
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields carr-mtx.del-zone carr-mtx.carrier carr-mtx.del-dscr carr-mtx.del-zip
&Scoped-define frame-title Carrier Zone Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname carr-mtx.
&Scoped-define window-size 24
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 37
&Scoped-define btn-cancel-col 26
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(5)
&Scoped-define FLDNAME1 carr-mtx.del-zone
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Zone
&Global-define FORMAT-2 x(5)
&Scoped-define FLDNAME2 carr-mtx.carrier
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Carrier

{methods/lookup.i}
