/* jobnumbr.p - Generated 10/17/2017 -  7:54 pm by NoSweat
"jobnumbr. " ~
"ASI " ~
"job-mch " ~
"job-mch.company = gcompany AND job-mch.m-code = s-machine " ~
"job-no " ~
"4 " ~
"19 " ~
"46 " ~
"job-no,job-no2 " ~
"Job Number " ~
"yes " ~
"job-no " ~
"Job Number Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{methods/defines/jobmach.i} ~{sys/inc/var.i new shared} " ~
"{touch/findjob.i} " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file job-mch
&Scoped-define where-statement job-mch.company = gcompany AND job-mch.m-code = s-machine
&Scoped-define return-field job-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields job-mch.job-no job-mch.job-no2
&Scoped-define show-fields-yellow job-mch.job-no LABEL-BGCOLOR 14 job-mch.job-no2 LABEL-BGCOLOR 14
&Scoped-define frame-title Job Number Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{methods/defines/jobmach.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include ~{touch/findjob.i}
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
&Scoped-define FLDNAME1 job-mch.job-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Job Number

{methods/lookup.i}
