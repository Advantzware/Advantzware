/* zipcode.p - Generated 06/12/1998 -  8:38 pm by NOSWEAT
"zipcode. " ~
"NOSWEAT " ~
"zipcode " ~
" " ~
"zipcode " ~
"4 " ~
"19 " ~
"62 " ~
"zipcode,pref_type,pref#,city,state " ~
"Zip/Postal Code,City " ~
"yes " ~
"zipcode,city " ~
"Zip Codes Lookup " ~
" " ~
"{methods/defines/s-pref.i} ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{methods/lookups/setpref.i} ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED} " ~
"zipcode. " ~
*/

&Scoped-define lookup-db NOSWEAT.
&Scoped-define lookup-file zipcode
&Scoped-define where-statement TRUE
&Scoped-define return-field zipcode
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 62
&Scoped-define show-fields zipcode.zipcode zipcode.pref_type zipcode.pref# zipcode.city zipcode.state
&Scoped-define frame-title Zip Codes Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} 
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
&Scoped-define end-include
&Scoped-define ui-prgmname zipcode.
&Scoped-define window-size 23
&Scoped-define window-col 44
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 56
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 53
&Scoped-define btn-cancel-col 42
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 X(10)
&Scoped-define FLDNAME1 zipcode.zipcode
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Zip/Postal Code
&Global-define FORMAT-2 X(20)
&Scoped-define FLDNAME2 zipcode.city
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 City

{methods/lookup.i}
