/* ui_lkup.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"ui_lkup. " ~
"ASI " ~
"prgrms " ~
"INDEX(prgrms.prgmname,'.') NE 0 " ~
"prgmname " ~
"4 " ~
"16 " ~
"46 " ~
"prgmname,prgtitle " ~
"Program,Title " ~
"yes " ~
"prgmname,prgtitle " ~
"User Interface Programs Lookup " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file prgrms
&Scoped-define where-statement INDEX(prgrms.prgmname,'.') NE 0
&Scoped-define return-field prgmname
&Scoped-define font 4
&Scoped-define height-size 16
&Scoped-define width-size 46
&Scoped-define show-fields prgrms.prgmname prgrms.prgtitle
&Scoped-define show-fields-yellow prgrms.prgmname LABEL-BGCOLOR 14 prgrms.prgtitle LABEL-BGCOLOR 14
&Scoped-define frame-title User Interface Programs Lookup
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 21
&Scoped-define window-col 52
&Scoped-define rect-1-row 17.15
&Scoped-define by-row 17.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 17.42
&Scoped-define btn-row 18.7
&Scoped-define btn-ok-col 39
&Scoped-define btn-cancel-col 32
&Scoped-define auto-find-row 20.65

&Global-define FORMAT-1 X(10)
&Scoped-define FLDNAME1 prgrms.prgmname
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Program
&Global-define FORMAT-2 X(30)
&Scoped-define FLDNAME2 prgrms.prgtitle
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Title

{methods/lookup.i}
