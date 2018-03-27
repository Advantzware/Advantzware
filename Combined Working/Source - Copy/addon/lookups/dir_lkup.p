/* dir_lkup.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"dir_lkup. " ~
" " ~
"ttbldir " ~
" " ~
"dirname " ~
"4 " ~
"19 " ~
"46 " ~
"dirname " ~
"Directory " ~
"no " ~
"dirname " ~
"Directory Lookup " ~
"{methods/lookups/ttbldir.i} ~{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{methods/defines/ttbldir.i} ~{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/

DEFINE INPUT-OUTPUT PARAMETER m-lookup-var AS CHARACTER.

&Scoped-define lookup-db  
&Scoped-define lookup-file ttbldir
&Scoped-define where-statement TRUE
&Scoped-define return-field dirname
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields ttbldir.dirname
&Scoped-define show-fields-yellow ttbldir.dirname LABEL-BGCOLOR 14
&Scoped-define frame-title Directory Lookup
&Scoped-define top-include ~{methods/lookups/ttbldir.i} ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{methods/defines/ttbldir.i} ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
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

&Scoped-define FLDNAME1 ttbldir.dirname
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Directory

{methods/lookup.i}
