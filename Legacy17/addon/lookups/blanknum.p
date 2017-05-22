/* blanknum.p - Generated 07/11/2000 -  5:22 pm by NoSweat
"blanknum. " ~
"ASI " ~
"job-mch " ~
"job-mch.company = gcompany AND job-mch.m-code = s-machine AND job-mch.job-no = s-job_number AND job-mch.job-no2 = INTEGER(s-job_sub) AND job-mch.blank-no = INTEGER(s-blank_number) " ~
"blank-no " ~
"4 " ~
"19 " ~
"46 " ~
"blank-no,job-no,job-no2,frm " ~
"BLANK " ~
"yes " ~
"blank-no " ~
"Job Blank Number Lookup " ~
"{custom/getcmpny.i} ~{sys/inc/varasgn.i} " ~
"{custom/gcompany.i} ~{methods/defines/jobmach.i} ~{sys/inc/var.i new shared}" ~
" " ~
" " ~
*/

&Scoped-define lookup-db ASI.
&Scoped-define lookup-file job-mch
&Scoped-define where-statement job-mch.company = gcompany AND job-mch.m-code = s-machine AND job-mch.job-no = s-job_number AND job-mch.job-no2 = INTEGER(s-job_sub) AND job-mch.blank-no = INTEGER(s-blank_number)
&Scoped-define return-field blank-no
&Scoped-define font 4
&Scoped-define height-size 19
&Scoped-define width-size 46
&Scoped-define show-fields job-mch.blank-no job-mch.job-no job-mch.job-no2 job-mch.frm
&Scoped-define frame-title Job Blank Number Lookup
&Scoped-define top-include ~{custom/getcmpny.i} ~{sys/inc/varasgn.i}
&Scoped-define def-include ~{custom/gcompany.i} ~{methods/defines/jobmach.i} ~{sys/inc/var.i new shared}
&Scoped-define end-include 
&Scoped-define ui-prgmname 
&Scoped-define window-size 23
&Scoped-define window-col 52
&Scoped-define rect-1-row 20.15
&Scoped-define by-row 20.42
&Scoped-define browse-order-width 40
&Scoped-define browse-order-row 20.42
&Scoped-define btn-row 21.77
&Scoped-define btn-ok-col 37
&Scoped-define btn-cancel-col 26
&Scoped-define auto-find-row 22.85

&Global-define DATATYP1 INTEGER
&Global-define FORMAT-1 >9
&Scoped-define FLDNAME1 job-mch.blank-no
&Scoped-define SORTBY-1 BY {&FLDNAME1}
&Scoped-define DESCRIP1 BLANK

{methods/lookup.i}
