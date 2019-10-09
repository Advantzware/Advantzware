/* titlcode.p - Generated 03/30/2005 -  3:32 pm by NoSweat
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
"description " ~
"Title Codes Search " ~
" " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"titlcode. " ~
*/

&Scoped-define search-db NOSWEAT.
&Scoped-define search-file titlcode
&Scoped-define where-statement TRUE
&Scoped-define return-field titlcode
&Scoped-define font 4
&Scoped-define height-size 10
&Scoped-define width-size 46
&Scoped-define show-fields titlcode.titlcode titlcode.description
&Scoped-define frame-title Title Codes Search
&Scoped-define search-text-row 11
&Scoped-define word-search-row 12
&Scoped-define btn-search-col 26
&Scoped-define top-include 
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname titlcode.
&Scoped-define window-size 16
&Scoped-define window-col 52
&Scoped-define rect-1-row 13.15
&Scoped-define by-row 13.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 13.42
&Scoped-define btn-row 14.77
&Scoped-define btn-ok-col 33
&Scoped-define btn-cancel-col 22
&Scoped-define auto-find-row 15.85

&Global-define FORMAT-1 X(8)
&Scoped-define FLDNAME1 titlcode.titlcode
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Title Code
&Global-define FORMAT-2 X(30)
&Scoped-define FLDNAME2 titlcode.description
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD description

{methods/search.i}
