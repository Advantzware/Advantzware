/* ventype.p - Generated 01/18/2000 -  4:46 pm by nosweat
"ventype. " ~
"ASI " ~
"ventype " ~
"ventype.company = gcompany " ~
"type " ~
"4 " ~
"17 " ~
"48 " ~
"type,Dscr " ~
"Vendor Type,Description " ~
"yes " ~
"type,Dscr " ~
"Dscr " ~
"Vendor Types Search " ~
"{custom/getcmpny.i} " ~
"{custom/gcompany.i} " ~
" " ~
"ventype. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file ventype
&Scoped-define where-statement ventype.company = gcompany
&Scoped-define return-field type
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 48
&Scoped-define show-fields ventype.type ventype.Dscr
&Scoped-define frame-title Vendor Types Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 28
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname ventype.
&Scoped-define window-size 23
&Scoped-define window-col 51
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 42
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 35
&Scoped-define btn-cancel-col 24
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(8)
&Scoped-define FLDNAME1 ventype.type
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Vendor Type
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 ventype.Dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD Dscr

{methods/search.i}
