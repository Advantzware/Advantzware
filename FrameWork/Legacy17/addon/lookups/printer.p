/* printer.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"printer. " ~
"ASI " ~
"printer " ~
" " ~
"pr-no " ~
"2 " ~
"19 " ~
"46 " ~
"pr-no,pr-name " ~
"Printer #,Printer Name " ~
"yes " ~
"pr-no,pr-name " ~
" " ~
" " ~
" ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}" ~
" ~{custom/gcompany.i} ~{sys/inc/var.i new shared}" ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file printer
&Scoped-define where-statement TRUE
&Scoped-define return-field pr-no
&Scoped-define font 2
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields printer.pr-no printer.pr-name
&Scoped-define show-fields-yellow printer.pr-no LABEL-BGCOLOR 14 printer.pr-name LABEL-BGCOLOR 14
&Scoped-define frame-title 
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 24
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 39
&Scoped-define btn-cancel-col 32
&Scoped-define auto-find-row 23.65

&Global-define DATATYP1 INTEGER
&Global-define FORMAT-1 >9
&Scoped-define FLDNAME1 printer.pr-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Printer #
&Global-define FORMAT-2 x(20)
&Scoped-define FLDNAME2 printer.pr-name
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Printer Name

{methods/lookup.i}
