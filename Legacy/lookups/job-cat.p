/* job-cat.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"job-cat. " ~
"ASI " ~
"job-cat " ~
" " ~
"cat " ~
"4 " ~
"19 " ~
"59 " ~
"cat,dscr " ~
"Category,Desc " ~
"yes " ~
"cat,dscr " ~
"Job Categories Lookup " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"job-cat. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file job-cat
&Scoped-define where-statement TRUE
&Scoped-define return-field cat
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 59
&Scoped-define show-fields job-cat.cat job-cat.dscr
&Scoped-define show-fields-yellow job-cat.cat LABEL-BGCOLOR 14 job-cat.dscr LABEL-BGCOLOR 14
&Scoped-define frame-title Job Categories Lookup
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname job-cat.
&Scoped-define window-size 24
&Scoped-define window-col 45.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 53
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 52
&Scoped-define btn-cancel-col 45
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(3)
&Scoped-define FLDNAME1 job-cat.cat
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Category
&Global-define FORMAT-2 x(45)
&Scoped-define FLDNAME2 job-cat.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Desc

{methods/lookup.i}
