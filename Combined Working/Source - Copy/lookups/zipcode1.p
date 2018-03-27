/* zipcode1.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"zipcode1. " ~
"ASI " ~
"zipcode " ~
" " ~
"zipcode " ~
"4 " ~
"19 " ~
"46 " ~
"zipcode,state,city " ~
"Zip/Postal Code " ~
"yes " ~
"zipcode " ~
"Zip Code Lookup " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"zipcode1. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file zipcode
&Scoped-define where-statement TRUE
&Scoped-define return-field zipcode
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields zipcode.zipcode zipcode.state zipcode.city
&Scoped-define show-fields-yellow zipcode.zipcode LABEL-BGCOLOR 14 zipcode.state LABEL-BGCOLOR 14 zipcode.city LABEL-BGCOLOR 14
&Scoped-define frame-title Zip Code Lookup
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname zipcode1.
&Scoped-define window-size 24
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 39
&Scoped-define btn-cancel-col 32
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 X(10)
&Scoped-define FLDNAME1 zipcode.zipcode
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Zip/Postal Code

{methods/lookup.i}
