/* bolh.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"bolh. " ~
"ASI " ~
"oe-bolh " ~
"oe-bolh.company eq g_company and oe-bolh.posted eq no and oe-bolh.deleted = no " ~
"bol-no " ~
"4 " ~
"19 " ~
"46 " ~
"bol-no,bol-date,cust-no " ~
"BOL#,BOL Date,Customer " ~
"no " ~
"bol-no,bol-date,cust-no " ~
" " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} ~{custom/gcompany.i} " ~
" " ~
"bolh. " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file oe-bolh
&Scoped-define where-statement oe-bolh.company eq g_company and oe-bolh.posted eq no and oe-bolh.deleted = no
&Scoped-define return-field bol-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields oe-bolh.bol-no oe-bolh.bol-date oe-bolh.cust-no
&Scoped-define show-fields-yellow oe-bolh.bol-no LABEL-BGCOLOR 14 oe-bolh.bol-date LABEL-BGCOLOR 14 oe-bolh.cust-no LABEL-BGCOLOR 14
&Scoped-define frame-title 
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared} ~{custom/gcompany.i}
&Scoped-define end-include 
&Scoped-define ui-prgmname bolh.
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
&Global-define FORMAT-1 >>>>>>>9
&Scoped-define FLDNAME1 oe-bolh.bol-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 BOL#
&Global-define DATATYP2 DATE
&Global-define FORMAT-2 99/99/9999
&Scoped-define FLDNAME2 oe-bolh.bol-date
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 BOL Date
&Global-define FORMAT-3 x(8)
&Scoped-define FLDNAME3 oe-bolh.cust-no
&Scoped-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-2}
&Scoped-define DESCRIP3 Customer

{methods/lookup.i}
