/* item1__.p - Generated 02/14/2001 -  5:13 pm by nosweat
"item1__. " ~
"ASI " ~
"item " ~
" " ~
"i-no " ~
"4 " ~
"19 " ~
"46 " ~
"i-no,i-name,i-dscr " ~
"Item No,Name " ~
"yes " ~
"i-no,i-name,? " ~
" " ~
"  ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}" ~
"  ~{custom/gcompany.i} ~{sys/inc/var.i new shared}" ~
" " ~
"itemall " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file item
&Scoped-define where-statement TRUE
&Scoped-define return-field i-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields item.i-no item.i-name item.i-dscr
&Scoped-define frame-title 
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname itemall
&Scoped-define window-size 24
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 37
&Scoped-define btn-cancel-col 26
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(10)
&Scoped-define FLDNAME1 item.i-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Item No
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 item.i-name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Name

{methods/lookup.i}
