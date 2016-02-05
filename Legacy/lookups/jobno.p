/* jobno.p - Generated 02/11/2004 - 10:53 am by nosweat
"jobno. " ~
"asi " ~
"job-hdr " ~
" " ~
"job-no " ~
"4 " ~
"19 " ~
"55 " ~
"job-no,job-no2,est-no,ftick-prnt " ~
"Job Number,Factory Ticket Printed " ~
"yes " ~
"job-no,ftick-prnt " ~
"Job# Lookups " ~
"{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared} " ~
" " ~
"jobno " ~
*/

&Scoped-define lookup-db asi.
&Scoped-define lookup-file job-hdr
&Scoped-define where-statement job-hdr.company eq cocode
&Scoped-define return-field job-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 55
&Scoped-define show-fields job-hdr.job-no job-hdr.job-no2 job-hdr.est-no job-hdr.ftick-prnt
&Scoped-define frame-title Job# Lookups
&Scoped-define top-include ~{custom/getcmpny.i} ~{custom/getloc.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{custom/gloc.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname jobno
&Scoped-define window-size 23
&Scoped-define window-col 47.5
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 49
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 46
&Scoped-define btn-cancel-col 35
&Scoped-define auto-find-row 22.85

&Global-define FORMAT-1 x(6)
&Scoped-define FLDNAME1 job-hdr.job-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Job Number
&Global-define DATATYP2 LOGICAL
&Global-define FORMAT-2 yes/no
&Scoped-define FLDNAME2 job-hdr.ftick-prnt
&Scoped-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Scoped-define DESCRIP2 Factory Ticket Printed
&SCOPED-DEFINE useMatches YES

{methods/lookup.i}
