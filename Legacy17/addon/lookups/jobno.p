/* jobno.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"jobno. " ~
"ASI " ~
"job-hdr " ~
"job-header.company = gcompany and job-hdr.opened = yes " ~
"job-no " ~
"4 " ~
"19 " ~
"46 " ~
"job-no,job-no2,est-no,ftick-prnt " ~
"Job Number,Factory Ticket Printed " ~
"yes " ~
"job-no,ftick-prnt " ~
"Job# Lookups " ~
"{custom/getcmpny.i}~{custom/getloc.i}~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i}~{custom/gloc.i}~{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file job-hdr
&Scoped-define where-statement job-hdr.company = gcompany and job-hdr.opened = yes
&Scoped-define return-field job-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields job-hdr.job-no job-hdr.job-no2 job-hdr.est-no job-hdr.ftick-prnt
&Scoped-define show-fields-yellow job-hdr.job-no LABEL-BGCOLOR 14 job-hdr.job-no2 LABEL-BGCOLOR 14 job-hdr.est-no LABEL-BGCOLOR 14 job-hdr.ftick-prnt LABEL-BGCOLOR 14
&Scoped-define frame-title Job# Lookups
&Scoped-define top-include ~{custom/getcmpny.i}~{custom/getloc.i}~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i}~{custom/gloc.i}~{sys/inc/var.i new shared}
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

&Global-define FORMAT-1 x(6)
&Scoped-define FLDNAME1 job-hdr.job-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Job Number
&Global-define DATATYP2 LOGICAL
&Global-define FORMAT-2 yes/no
&Scoped-define FLDNAME2 job-hdr.ftick-prnt
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Factory Ticket Printed

{methods/lookup.i}
