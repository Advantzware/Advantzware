/* item_.p - Generated 02/01/2000 -  3:44 pm by nosweat
"item_. " ~
"ASI " ~
"item " ~
"item.company = gcompany " ~
"i-name " ~
"4 " ~
"19 " ~
"77 " ~
"i-name,i-no,i-dscr " ~
"Name,Item No,Desc " ~
"yes " ~
"i-name,i-no,i-dscr " ~
"Raw Materials Inventory Name Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"item. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file item
&Scoped-define where-statement item.company = gcompany
&Scoped-define return-field i-name
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 77
&Scoped-define show-fields item.i-name item.i-no item.i-dscr
&Scoped-define frame-title Raw Materials Inventory Name Lookup
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
&Scoped-define btn-ok-col 68
&Scoped-define btn-cancel-col 57
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(30)
&Scoped-define FLDNAME1 item.i-name
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Name
&Global-define FORMAT-2 x(10)
&Scoped-define FLDNAME2 item.i-no
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Item No
&Global-define FORMAT-3 x(30)
&Scoped-define FLDNAME3 item.i-dscr
&Scoped-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-2}
&Scoped-define DESCRIP3 Desc

{methods/lookup.i}
