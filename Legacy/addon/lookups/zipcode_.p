/* zipcode_.p - Generated 03/27/1998 - 10:15 pm by Exitt
"zipcode_. " ~
"NOSWEAT " ~
"zipcode " ~
" " ~
"city " ~
"4 " ~
"19 " ~
"51 " ~
"city,zipcode,state " ~
"City,Zip/Postal Code " ~
"yes " ~
"city,zipcode " ~
"City Lookup " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
" " ~
"zipcode. " ~
*/

&Scoped-define lookup-db NOSWEAT.
&Scoped-define lookup-file zipcode
&Scoped-define where-statement TRUE
&Scoped-define return-field city
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 51
&Scoped-define show-fields zipcode.city zipcode.zipcode zipcode.state
&Scoped-define frame-title City Lookup
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
&Scoped-define btn-ok-col 42
&Scoped-define btn-cancel-col 31
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 X(20)
&Scoped-define FLDNAME1 zipcode.city
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 City
&Global-define FORMAT-2 X(10)
&Scoped-define FLDNAME2 zipcode.zipcode
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Zip/Postal Code

{methods/lookup.i}
