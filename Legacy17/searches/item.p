/* item.p - Generated 02/01/2000 -  3:42 pm by nosweat
"item. " ~
"ASI " ~
"item " ~
"item.company = gcompany " ~
"i-no " ~
"4 " ~
"17 " ~
"77 " ~
"i-no,i-name,i-dscr " ~
"Item No,Name,Desc " ~
"yes " ~
"i-no,i-name,i-dscr " ~
"i-name " ~
"Raw Materials Inventory Search " ~
"{custom/getcmpny.i} " ~
"{custom/gcompany.i} " ~
" " ~
"item. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file item
&Scoped-define where-statement item.company = gcompany
&Scoped-define return-field i-no
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 77
&Scoped-define show-fields item.i-no item.i-name item.i-dscr
&Scoped-define frame-title Raw Materials Inventory Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 57
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname item.
&Scoped-define window-size 23
&Scoped-define window-col 36.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 71
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 64
&Scoped-define btn-cancel-col 53
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(10)
&Scoped-define FLDNAME1 item.i-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Item No
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 item.i-name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Name
&Global-define FORMAT-3 x(30)
&Scoped-define FLDNAME3 item.i-dscr
&Scoped-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-2}
&Scoped-define DESCRIP3 Desc
&Scoped-define WORDFLD i-name

{methods/search.i}
