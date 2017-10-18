/* jobno_.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"jobno_. " ~
"asi " ~
"job-hdr " ~
"job-hdr.company = gcompany and job-hdr.opened = yes " ~
"ftick-prnt " ~
"4 " ~
"19 " ~
"55 " ~
"job-no2,job-no,est-no,ftick-prnt " ~
"Factory Ticket Printed,Job Number " ~
"yes " ~
"ftick-prnt,job-no " ~
"Job# Description Lookups " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared} " ~
" " ~
"jobno1 " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file job-hdr
&Scoped-define where-statement job-hdr.company = gcompany AND job-hdr.job-no NE "" and job-hdr.opened = yes
&Scoped-define return-field ftick-prnt
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 55
&Scoped-define show-fields job-hdr.job-no2 job-hdr.job-no job-hdr.est-no job-hdr.ftick-prnt
&Scoped-define show-fields-yellow job-hdr.job-no2 LABEL-BGCOLOR 14 job-hdr.job-no LABEL-BGCOLOR 14 job-hdr.est-no LABEL-BGCOLOR 14 job-hdr.ftick-prnt LABEL-BGCOLOR 14
&Scoped-define frame-title Job# Description Lookups
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname jobno1
&Scoped-define window-size 24
&Scoped-define window-col 47.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 49
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.7
&Scoped-define btn-ok-col 48
&Scoped-define btn-cancel-col 41
&Scoped-define auto-find-row 23.65

&Global-define DATATYP1 LOGICAL
&Global-define FORMAT-1 yes/no
&Scoped-define FLDNAME1 job-hdr.ftick-prnt
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Factory Ticket Printed
&Global-define FORMAT-2 x(6)
&Scoped-define FLDNAME2 job-hdr.job-no
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Job Number

{methods/lookup.i}
