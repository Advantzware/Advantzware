/* zipcode.p - Generated 03/27/1998 - 10:21 pm by Exitt
"zipcode. " ~
"NOSWEAT " ~
"zipcode " ~
" " ~
"zipcode " ~
"4 " ~
"17 " ~
"51 " ~
"zipcode,city,state " ~
"Zip/Postal Code,City " ~
"yes " ~
"zipcode,city " ~
"city " ~
"Zip Code Search " ~
" ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
" ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
"zipcode. " ~
*/

&Scoped-define search-db NOSWEAT.
&Scoped-define search-file zipcode
&Scoped-define where-statement TRUE
&Scoped-define return-field zipcode
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 51
&Scoped-define show-fields zipcode.zipcode zipcode.city zipcode.state
&Scoped-define frame-title Zip Code Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 31
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} 
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} 
&Scoped-define end-include 
&Scoped-define ui-prgmname zipcode.
&Scoped-define window-size 23
&Scoped-define window-col 49.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 45
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 38
&Scoped-define btn-cancel-col 27
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 X(10)
&Scoped-define FLDNAME1 zipcode.zipcode
&Scoped-define DESCRIP1 Zip/Postal Code
&Global-define FORMAT-2 X(20)
&Scoped-define FLDNAME2 zipcode.city
&Scoped-define DESCRIP2 City
&Scoped-define WORDFLD zipcode.city

{methods/search.i}
