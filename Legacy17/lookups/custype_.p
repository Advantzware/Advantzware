/* custype_.p - Generated 01/20/2003 - 11:51 am by nosweat
"custype_. " ~
"ASI " ~
"mach " ~
"mach.company = gcompany " ~
"m-code " ~
"3 " ~
"19 " ~
"34 " ~
"m-code,m-dscr " ~
" " ~
"yes " ~
"m-code,m-dscr " ~
"Machine# Lookups " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"mach. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file mach
&Scoped-define where-statement mach.company = gcompany
&Scoped-define return-field m-code
&Scoped-define font 3
&Scoped-define height-size 19
&Scoped-define width-size 34
&Scoped-define show-fields mach.m-code mach.m-dscr
&Scoped-define frame-title Machine# Lookups
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname mach.
&Scoped-define window-size 24
&Scoped-define window-col 58
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 28
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 25
&Scoped-define btn-cancel-col 14
&Scoped-define auto-find-row 23.65


{methods/lookup.i}
