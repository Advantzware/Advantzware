/* po-ordl1.p - Generated 01/03/2001 -  8:49 am by nosweat
"po-ordl1. " ~
"ASI " ~
"po-ordl " ~
" " ~
"i-no " ~
"4 " ~
"19 " ~
"95 " ~
"i-no,i-name,job-no,job-no2,s-wid,s-len " ~
"Item# " ~
"yes " ~
"i-no " ~
" " ~
" " ~
" ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}" ~
" ~{custom/gcompany.i} ~{sys/inc/var.i new shared}" ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file po-ordl
&Scoped-define where-statement TRUE
&Scoped-define return-field i-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 95
&Scoped-define show-fields po-ordl.i-no po-ordl.i-name po-ordl.job-no po-ordl.job-no2 po-ordl.s-wid po-ordl.s-len
&Scoped-define frame-title 
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 23
&Scoped-define window-col 27.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 89
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 86
&Scoped-define btn-cancel-col 75
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(15)
&Scoped-define FLDNAME1 po-ordl.i-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Item#

{methods/lookup.i}
