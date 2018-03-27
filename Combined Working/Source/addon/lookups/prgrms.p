/* prgrms.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"prgrms. " ~
"ASI " ~
"prgrms " ~
" " ~
"prgtitle " ~
"4 " ~
"19 " ~
"46 " ~
"prgtitle,prgmname " ~
"Title,Program " ~
"yes " ~
"prgtitle,prgmname " ~
"Program Titles Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file prgrms
&Scoped-define where-statement TRUE
&Scoped-define return-field prgtitle
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields prgrms.prgtitle prgrms.prgmname
&Scoped-define show-fields-yellow prgrms.prgtitle LABEL-BGCOLOR 14 prgrms.prgmname LABEL-BGCOLOR 14
&Scoped-define frame-title Program Titles Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
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

&Global-define FORMAT-1 X(30)
&Scoped-define FLDNAME1 prgrms.prgtitle
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Title
&Global-define FORMAT-2 X(10)
&Scoped-define FLDNAME2 prgrms.prgmname
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Program

{methods/lookup.i}
