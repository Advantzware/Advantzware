/* formnumb.p - Generated 07/11/2000 -  5:21 pm by NoSweat
"formnumb. " ~
"ASI " ~
"job-mch " ~
"job-mch.company = gcompany AND job-mch.m-code = s-machine AND job-mch.job-no = s-job_number AND job-mch.job-no2 = INTEGER(s-job_sub) " ~
"frm " ~
"4 " ~
"19 " ~
"46 " ~
"frm,job-no,job-no2 " ~
"Form " ~
"yes " ~
"frm " ~
"Job Form Number Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{methods/defines/jobmach.i} ~{sys/inc/var.i new shared} " ~
" " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file job-mch
&Scoped-define where-statement job-mch.company = gcompany AND job-mch.m-code = s-machine AND job-mch.job-no = s-job_number AND job-mch.job-no2 = INTEGER(s-job_sub)
&Scoped-define return-field frm
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields job-mch.frm job-mch.job-no job-mch.job-no2
&Scoped-define frame-title Job Form Number Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{methods/defines/jobmach.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 24
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 37
&Scoped-define btn-cancel-col 26
&Scoped-define auto-find-row 23.65

&Global-define DATATYP1 INTEGER
&Global-define FORMAT-1 >>9
&Scoped-define FLDNAME1 job-mch.frm
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 Form

{methods/lookup.i}
