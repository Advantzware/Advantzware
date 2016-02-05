/* poino.p - Generated 08/07/2003 -  2:07 pm by nosweat
"poino. " ~
"asi " ~
"po-ordl " ~
" " ~
"i-no " ~
"4 " ~
"19 " ~
"80 " ~
"i-no,i-name,po-no " ~
"Item#,Name " ~
"yes " ~
"i-no,i-name " ~
"Item By PO# " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file po-ordl
&Scoped-define where-statement po-ordl.company eq cocode
&Scoped-define return-field i-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 80
&Scoped-define show-fields po-ordl.i-no po-ordl.i-name po-ordl.po-no
&Scoped-define frame-title Item By PO#
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 23
&Scoped-define window-col 35
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 74
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 71
&Scoped-define btn-cancel-col 60
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(15)
&Scoped-define FLDNAME1 po-ordl.i-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Item#
&Global-define FORMAT-2 x(30)
&Scoped-define FLDNAME2 po-ordl.i-name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Name

{methods/lookup.i}
