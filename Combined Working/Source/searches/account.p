/* account.p - Generated 01/18/2000 -  3:55 pm by nosweat
"account. " ~
"ASI " ~
"account " ~
"account.company = gcompany " ~
"actnum " ~
"4 " ~
"17 " ~
"76 " ~
"actnum,dscr " ~
"Account No,Description " ~
"no " ~
"actnum,dscr " ~
"dscr " ~
"G/L Accounts Search " ~
"{custom/getcmpny.i} " ~
"{custom/gcompany.i} " ~
" " ~
"account. " ~
*/

&Scoped-define search-db ASI.
&Scoped-define search-file account
&Scoped-define where-statement account.company = gcompany
&Scoped-define return-field actnum
&Scoped-define font 4
&Scoped-define height-size 17
&Scoped-define width-size 76
&Scoped-define show-fields account.actnum account.dscr
&Scoped-define frame-title G/L Accounts Search
&Scoped-define search-text-row 18
&Scoped-define word-search-row 19
&Scoped-define btn-search-col 56
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
&Scoped-define btn-ok-col 63
&Scoped-define btn-cancel-col 52
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(25)
&Scoped-define FLDNAME1 account.actnum
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Account No
&Global-define FORMAT-2 x(45)
&Scoped-define FLDNAME2 account.dscr
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Description
&Scoped-define WORDFLD dscr

{methods/search.i}
