/* emailcod.p - Generated 03/30/2005 -  3:29 pm by NoSweat
"emailcod. " ~
"ASI " ~
"emailcod " ~
" " ~
"emailcod " ~
"4 " ~
"19 " ~
"46 " ~
"emailcod,description " ~
"Email Code,Description " ~
"yes " ~
"emailcod,description " ~
"Email Codes Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"emailcod. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file emailcod
&Scoped-define where-statement TRUE
&Scoped-define return-field emailcod
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields emailcod.emailcod emailcod.description
&Scoped-define frame-title Email Codes Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname emailcod.
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

&Global-define FORMAT-1 X(8)
&Scoped-define FLDNAME1 emailcod.emailcod
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Email Code
&Global-define FORMAT-2 X(30)
&Scoped-define FLDNAME2 emailcod.description
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
