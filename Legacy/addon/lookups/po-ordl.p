/* po-ordl.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"po-ordl. " ~
"ASI " ~
"po-ordl " ~
"po-ordl.company eq gcompany " ~
"po-no " ~
"4 " ~
"19 " ~
"100 " ~
"po-no,due-date,job-no,job-no2,i-no,i-name " ~
"PO#,Item# " ~
"no " ~
"po-no,i-no " ~
"PO# " ~
"{custom/getcmpny.i}  ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{methods/defines/rm-rcpt.i} ~{sys/inc/var.i new shared} " ~
"{custom/itemno.i} " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file po-ordl
&Scoped-define where-statement po-ordl.company eq gcompany
&Scoped-define return-field po-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 100
&Scoped-define show-fields po-ordl.po-no po-ordl.due-date po-ordl.job-no po-ordl.job-no2 po-ordl.i-no po-ordl.i-name
&Scoped-define show-fields-yellow po-ordl.po-no LABEL-BGCOLOR 14 po-ordl.due-date LABEL-BGCOLOR 14 po-ordl.job-no LABEL-BGCOLOR 14 po-ordl.job-no2 LABEL-BGCOLOR 14 po-ordl.i-no LABEL-BGCOLOR 14 po-ordl.i-name LABEL-BGCOLOR 14
&Scoped-define frame-title PO#
&Scoped-define top-include ~{custom/getcmpny.i}  ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{methods/defines/rm-rcpt.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include ~{custom/itemno.i}
&Scoped-define ui-prgmname 
&Scoped-define window-size 24
&Scoped-define window-col 25
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 94
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 93
&Scoped-define btn-cancel-col 86
&Scoped-define auto-find-row 23.65

&Global-define DATATYP1 INTEGER
&Global-define FORMAT-1 >>>>>9
&Scoped-define FLDNAME1 po-ordl.po-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 PO#
&Global-define FORMAT-2 x(15)
&Scoped-define FLDNAME2 po-ordl.i-no
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Item#

{methods/lookup.i}
