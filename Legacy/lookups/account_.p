/* account_.p - Generated 07/16/2002 -  8:29 am by nosweat
"account_. " ~
" " ~
" " ~
"account.company = gcompany " ~
" " ~
"4 " ~
"19 " ~
"76 " ~
"dscr,actnum,type " ~
"Account No,Description " ~
"no " ~
"dscr,actnum " ~
"G/L Accounts Description Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"account. " ~
*/

DEFINE INPUT-OUTPUT PARAMETER m-lookup-var AS CHARACTER.

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file account
&Scoped-define where-statement account.company = gcompany
&Scoped-define return-field actnum
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 76
&Scoped-define show-fields account.dscr account.actnum account.type
&Scoped-define frame-title G/L Accounts Description Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname account.
&Scoped-define window-size 23
&Scoped-define window-col 37
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 70
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 67
&Scoped-define btn-cancel-col 56
&Scoped-define auto-find-row 22.85

&Scoped-define FLDNAME1 account.dscr
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Account No
&Scoped-define FLDNAME2 account.actnum
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description

{methods/lookup.i}
