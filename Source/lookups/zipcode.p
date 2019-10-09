/* zipcode.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"zipcode. " ~
"ASI " ~
"zipcode " ~
" " ~
"zipcode " ~
"4 " ~
"19 " ~
"62 " ~
"zipcode,pref_type,pref#,city,state " ~
"Zip/Postal Code,City " ~
"yes " ~
"zipcode,city " ~
"Zip Codes Lookup " ~
"{sys/inc/varasgn.i} " ~
"{methods/defines/s-pref.i} ~{sys/inc/var.i new shared} " ~
"{methods/lookups/setpref.i} " ~
"zipcode. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file zipcode
&Scoped-define where-statement TRUE
&Scoped-define return-field zipcode
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 62
&Scoped-define show-fields zipcode.zipcode zipcode.pref_type zipcode.pref# zipcode.city zipcode.state
&Scoped-define show-fields-yellow zipcode.zipcode LABEL-BGCOLOR 14 zipcode.pref_type LABEL-BGCOLOR 14 zipcode.pref# LABEL-BGCOLOR 14 zipcode.city LABEL-BGCOLOR 14 zipcode.state LABEL-BGCOLOR 14
&Scoped-define frame-title Zip Codes Lookup
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{methods/defines/s-pref.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include ~{methods/lookups/setpref.i}
&Scoped-define ui-prgmname zipcode.
&Scoped-define window-size 24
&Scoped-define window-col 44
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 56
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 55
&Scoped-define btn-cancel-col 48
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 X(10)
&Scoped-define FLDNAME1 zipcode.zipcode
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Zip/Postal Code
&Global-define FORMAT-2 X(20)
&Scoped-define FLDNAME2 zipcode.city
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 City

{methods/lookup.i}
