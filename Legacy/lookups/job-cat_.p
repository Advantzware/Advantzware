/* job-cat_.p - Generated 01/18/2000 - 11:52 am by nosweat
"job-cat_. " ~
"ASI " ~
"job-cat " ~
" " ~
"dscr " ~
"4 " ~
"19 " ~
"59 " ~
"dscr,cat " ~
"Desc,Category " ~
"yes " ~
"dscr,cat " ~
"Job Categories Description Lookup " ~
"{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} " ~
" " ~
"job-cat. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file job-cat
&Scoped-define where-statement TRUE
&Scoped-define return-field dscr
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 59
&Scoped-define show-fields job-cat.dscr job-cat.cat
&Scoped-define frame-title Job Categories Description Lookup
&Scoped-define top-include ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname job-cat.
&Scoped-define window-size 23
&Scoped-define window-col 45.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 53
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 50
&Scoped-define btn-cancel-col 39
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(45)
&Scoped-define FLDNAME1 job-cat.dscr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Desc
&Global-define FORMAT-2 x(3)
&Scoped-define FLDNAME2 job-cat.cat
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Category

{methods/lookup.i}
