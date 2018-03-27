/* relno.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"relno. " ~
"ASI " ~
"oe-relh " ~
"oe-relh.company = cocode and not oe-relh.posted " ~
"release# " ~
"4 " ~
"19 " ~
"46 " ~
"release#,printed,rel-date " ~
"Release# " ~
"yes " ~
"release# " ~
" " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"relno. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file oe-relh
&Scoped-define where-statement oe-relh.company = cocode and not oe-relh.posted
&Scoped-define return-field release#
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields oe-relh.release# oe-relh.printed oe-relh.rel-date
&Scoped-define show-fields-yellow oe-relh.release# LABEL-BGCOLOR 14 oe-relh.printed LABEL-BGCOLOR 14 oe-relh.rel-date LABEL-BGCOLOR 14
&Scoped-define frame-title 
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname relno.
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

&Global-define DATATYP1 INTEGER
&Global-define FORMAT-1 ->,>>>,>>9
&Scoped-define FLDNAME1 oe-relh.release#
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Release#

{methods/lookup.i}
