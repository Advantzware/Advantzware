/* titlcode.p - Generated 07/28/1998 -  8:08 pm by NoSweat
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
" ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
" ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
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
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} 
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} 
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
&Scoped-define DESCRIP1 Title Code
&Global-define FORMAT-2 X(30)
&Scoped-define FLDNAME2 titlcode.description
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD titlcode.description

{methods/search.i}
