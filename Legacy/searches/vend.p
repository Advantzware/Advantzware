/* vend.p - Generated 01/26/2000 -  1:47 pm by nosweat
"vend. " ~
"ASI " ~
"vend " ~
"vend.company = gcompany " ~
"vend-no " ~
"4 " ~
"17 " ~
"44 " ~
"vend-no,name " ~
"Vendor,Name " ~
"no " ~
"vend-no,name " ~
"name " ~
"Vendors Search " ~
"{custom/getcmpny.i} " ~
"{custom/gcompany.i} " ~
" " ~
"vend. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file vend
&Scoped-define where-statement vend.company = gcompany
&Scoped-define return-field vend-no
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 44
&Scoped-define show-fields vend.vend-no vend.name
&Scoped-define frame-title Vendors Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 24
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i NEW SHARED}
/*&Scoped-define top-include ~{custom/getcmpny.i}
&Scoped-define def-include ~{custom/gcompany.i}*/
&Scoped-define end-include 
&Scoped-define ui-prgmname vend.
&Scoped-define window-size 23
&Scoped-define window-col 53
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 38
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 31
&Scoped-define btn-cancel-col 20
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(8)
&Scoped-define FLDNAME1 vend.vend-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Vendor
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 vend.name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Name
&Scoped-define WORDFLD name

{methods/search.i}
