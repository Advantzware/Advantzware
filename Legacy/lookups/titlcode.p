/* titlcode.p - Generated 03/30/2005 -  3:30 pm by NoSweat
"titlcode. " ~
"NOSWEAT " ~
"titlcode " ~
" " ~
"titlcode " ~
"4 " ~
"10 " ~
"46 " ~
"titlcode,description " ~
"Title Code,Description " ~
"yes " ~
"titlcode,description " ~
"Title Codes Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"titlcode. " ~
*/

&Scoped-define lookup-db NOSWEAT.
&Scoped-define lookup-file titlcode
&Scoped-define where-statement TRUE
&Scoped-define return-field titlcode
&Scoped-define font 4
&Scoped-define height-size 10
&Scoped-define width-size 46
&Scoped-define show-fields titlcode.titlcode titlcode.description
&Scoped-define frame-title Title Codes Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname titlcode.
&Scoped-define window-size 14
&Scoped-define window-col 52
&Scoped-define rect-1-row 11.15
&Scoped-define by-row 11.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 11.42
&Scoped-define btn-row 12.77
&Scoped-define btn-ok-col 37
&Scoped-define btn-cancel-col 26
&Scoped-define auto-find-row 13.85

&Global-define FORMAT-1 X(8)
&Scoped-define FLDNAME1 titlcode.titlcode
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Title Code
&Global-define FORMAT-2 X(30)
&Scoped-define FLDNAME2 titlcode.description
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
