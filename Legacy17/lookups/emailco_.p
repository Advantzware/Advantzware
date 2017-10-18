/* emailco_.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"emailco_. " ~
"ASI " ~
"emailcod " ~
" " ~
"description " ~
"4 " ~
"19 " ~
"46 " ~
"description,emailcod " ~
"Description,Email Code " ~
"yes " ~
"description,emailcod " ~
"Email Codes Description Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"emailcod. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file emailcod
&Scoped-define where-statement TRUE
&Scoped-define return-field description
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields emailcod.description emailcod.emailcod
&Scoped-define show-fields-yellow emailcod.description LABEL-BGCOLOR 14 emailcod.emailcod LABEL-BGCOLOR 14
&Scoped-define frame-title Email Codes Description Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname emailcod.
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
&Scoped-define FLDNAME1 emailcod.description
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description
&Global-define FORMAT-2 X(8)
&Scoped-define FLDNAME2 emailcod.emailcod
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Email Code

{methods/lookup.i}
