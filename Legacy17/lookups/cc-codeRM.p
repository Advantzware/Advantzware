/* cc-codeRM.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"cc-codeRM. " ~
" " ~
"ttblcc-code " ~
" " ~
"cc-code " ~
"4 " ~
"19 " ~
"46 " ~
"cc-code " ~
"Cycle Count Code " ~
"no " ~
"cc-code " ~
"RM Cycle Count Code Lookup " ~
"{custom/getcmpny.i} ~{methods/lookups/cc-code.i item} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{methods/defines/cc-code.i item} ~{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/

DEFINE INPUT-OUTPUT PARAMETER m-lookup-var AS CHARACTER.

&Scoped-define lookup-db  
&Scoped-define lookup-file ttblcc-code
&Scoped-define where-statement TRUE
&Scoped-define return-field cc-code
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields ttblcc-code.cc-code
&Scoped-define show-fields-yellow ttblcc-code.cc-code LABEL-BGCOLOR 14
&Scoped-define frame-title RM Cycle Count Code Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{methods/lookups/cc-code.i item} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{methods/defines/cc-code.i item} ~{sys/inc/var.i new shared}
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

&Scoped-define FLDNAME1 ttblcc-code.cc-code
&Scoped-define FORMAT-1 X(8)
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Cycle Count Code

{methods/lookup.i}
