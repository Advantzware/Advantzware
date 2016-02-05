/* bolnum.p - Generated 01/29/2003 -  2:57 pm by nosweat
"bolnum. " ~
"ASI " ~
"oe-bolh " ~
" " ~
"bol-no " ~
"4 " ~
"19 " ~
"46 " ~
"bol-no " ~
" " ~
"no " ~
"bol-no " ~
" " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file oe-bolh
&Scoped-define where-statement oe-bolh.company eq cocode
&Scoped-define return-field bol-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields oe-bolh.bol-no
&Scoped-define frame-title 
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 23
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 37
&Scoped-define btn-cancel-col 26
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 >>>>>>>>
&Scoped-define DATATYP1 INT
&Scoped-define FLDNAME1 oe-bolh.bol-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 BOL#

{methods/lookup.i}
