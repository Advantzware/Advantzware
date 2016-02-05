/* emailcod.p - Generated 03/30/2005 -  3:31 pm by NoSweat
"emailcod. " ~
"ASI " ~
"emailcod " ~
" " ~
"description " ~
"4 " ~
"17 " ~
"46 " ~
"emailcod,description " ~
"Email Code,Description " ~
"yes " ~
"emailcod,description " ~
"description " ~
"Email Codes Search " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"emailcod. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file emailcod
&Scoped-define where-statement TRUE
&Scoped-define return-field description
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 46
&Scoped-define show-fields emailcod.emailcod emailcod.description
&Scoped-define frame-title Email Codes Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 26
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
&Scoped-define btn-ok-col 33
&Scoped-define btn-cancel-col 22
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 X(8)
&Scoped-define FLDNAME1 emailcod.emailcod
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Email Code
&Global-define FORMAT-2 X(30)
&Scoped-define FLDNAME2 emailcod.description
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD description

{methods/search.i}
