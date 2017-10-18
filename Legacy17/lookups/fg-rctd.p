/* fg-rctd.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"fg-rctd. " ~
"asi " ~
"fg-rctd " ~
"fg-rctd.company eq gcompany and index('""RSTACE""',fg-rctd.rita-code) gt 0 " ~
"r-no " ~
"4 " ~
"19 " ~
"100 " ~
"r-no,rct-date,tag,po-no,job-no,job-no2,i-no,t-qty,rita-code " ~
"Seq#,Tag#,PO#,Job#,FG Item# " ~
"no " ~
"r-no,tag,po-no,job-no,i-no " ~
"FG Transaction Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{sys/inc/var.i new shared} ~{custom/gcompany.i} " ~
" " ~
"fg-rctd. " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file fg-rctd
&Scoped-define where-statement fg-rctd.company eq gcompany and index('""RSTACE""',fg-rctd.rita-code) gt 0
&Scoped-define return-field r-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 100
&Scoped-define show-fields fg-rctd.r-no fg-rctd.rct-date fg-rctd.tag fg-rctd.po-no fg-rctd.job-no fg-rctd.job-no2 fg-rctd.i-no fg-rctd.t-qty fg-rctd.rita-code
&Scoped-define show-fields-yellow fg-rctd.r-no LABEL-BGCOLOR 14 fg-rctd.rct-date LABEL-BGCOLOR 14 fg-rctd.tag LABEL-BGCOLOR 14 fg-rctd.po-no LABEL-BGCOLOR 14 fg-rctd.job-no LABEL-BGCOLOR 14 fg-rctd.job-no2 LABEL-BGCOLOR 14 fg-rctd.i-no LABEL-BGCOLOR 14 fg-rctd.t-qty LABEL-BGCOLOR 14 fg-rctd.rita-code LABEL-BGCOLOR 14
&Scoped-define frame-title FG Transaction Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{sys/inc/var.i new shared} ~{custom/gcompany.i}
&Scoped-define end-include 
&Scoped-define ui-prgmname fg-rctd.
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
&Global-define FORMAT-1 >>>>>>>9
&Scoped-define FLDNAME1 fg-rctd.r-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Seq#
&Global-define FORMAT-2 x(8)
&Scoped-define FLDNAME2 fg-rctd.tag
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Tag#
&Global-define FORMAT-3 x(9)
&Scoped-define FLDNAME3 fg-rctd.po-no
&Scoped-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-2}
&Scoped-define DESCRIP3 PO#
&Global-define FORMAT-4 x(6)
&Scoped-define FLDNAME4 fg-rctd.job-no
&Scoped-define SORTBY-4 BY {&FLDNAME4} {&SORTBY-3}
&Scoped-define DESCRIP4 Job#
&Global-define FORMAT-5 x(10)
&Scoped-define FLDNAME5 fg-rctd.i-no
&Scoped-define SORTBY-5 BY {&FLDNAME5} {&SORTBY-4}
&Scoped-define DESCRIP5 FG Item#

{methods/lookup.i}
