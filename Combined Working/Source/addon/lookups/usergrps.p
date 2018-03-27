/* usergrps.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"usergrps. " ~
"ASI " ~
"usergrps " ~
" " ~
"usergrps " ~
"4 " ~
"19 " ~
"86 " ~
"usergrps,users " ~
"User Group " ~
"yes " ~
"usergrps " ~
"User Groups Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
"usergrps. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file usergrps
&Scoped-define where-statement TRUE
&Scoped-define return-field usergrps
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 86
&Scoped-define show-fields usergrps.usergrps usergrps.users
&Scoped-define show-fields-yellow usergrps.usergrps LABEL-BGCOLOR 14 usergrps.users LABEL-BGCOLOR 14
&Scoped-define frame-title User Groups Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include 
&Scoped-define ui-prgmname usergrps.
&Scoped-define window-size 24
&Scoped-define window-col 32
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 80
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 79
&Scoped-define btn-cancel-col 72
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 X(20)
&Scoped-define FLDNAME1 usergrps.usergrps
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 User Group

{methods/lookup.i}
