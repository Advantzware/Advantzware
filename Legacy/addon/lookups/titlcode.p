/* titlcode.p - Generated 07/02/2001 - 12:56 pm by nosweat
"titlcode. " ~
"NOSWEAT " ~
"titlcode " ~
" " ~
"titlcode " ~
"4 " ~
"10 " ~
"46 " ~
"description " ~
"Description " ~
"yes " ~
"description " ~
"Title Codes Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
"titlcode. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file titlcode
&Scoped-define where-statement TRUE
&Scoped-define return-field titlcode
&Scoped-define font 4
&Scoped-define height-size 10
&Scoped-define width-size 46
&Scoped-define show-fields titlcode.description
&Scoped-define frame-title Title Codes Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
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

&Global-define FORMAT-1 X(30)
&Scoped-define FLDNAME1 titlcode.description
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Description

{methods/lookup.i}
