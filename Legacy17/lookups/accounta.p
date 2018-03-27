/* accounta.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"accounta. " ~
"ASI " ~
"account " ~
"account.company eq gcompany and account.type eq 'A' " ~
"actnum " ~
"4 " ~
"19 " ~
"82 " ~
"actnum,dscr,type " ~
"Account No,Desc " ~
"no " ~
"actnum,dscr " ~
"GL Asset Accounts " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{sys/inc/var.i new shared} " ~
" " ~
"accountl. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file account
&Scoped-define where-statement account.company eq gcompany and account.type eq 'A'
&Scoped-define return-field actnum
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 82
&Scoped-define show-fields account.actnum account.dscr account.type
&Scoped-define show-fields-yellow account.actnum LABEL-BGCOLOR 14 account.dscr LABEL-BGCOLOR 14 account.type LABEL-BGCOLOR 14
&Scoped-define frame-title GL Asset Accounts
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname accountl.
&Scoped-define window-size 24
&Scoped-define window-col 34
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 76
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 75
&Scoped-define btn-cancel-col 68
&Scoped-define auto-find-row 23.65

&Global-define FORMAT-1 x(25)
&Scoped-define FLDNAME1 account.actnum
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Account No
&Global-define FORMAT-2 x(45)
&Scoped-define FLDNAME2 account.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Desc

{methods/lookup.i}
