/* job.i  override for job and job-hdr
"ASI" "job"
"job-no" "Job Number" ""
"start-date" "Start Date" ""
/* job-hdr */
"i-no" "FG Item#" ""
"est-no" "Estimate" ""
"stat" "Status" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"yes" "no" "no"
"no"
*/

&Global-define FLDNAME1 job.job-no
&Global-define FORMAT-1 X(6)
&Global-define SORTBY-1 by job.job-no descending
&Global-define IDXNAME1 
&Global-define DESCRIP1 Job Number
&Global-define FLDNAME2 job.start-date
&Global-define DATATYP2 DATE
&Global-define FORMAT-2 99/99/9999
&Global-define SORTBY-2 by job.start-date
&Global-define IDXNAME2 
&Global-define DESCRIP2 Start Date
&Global-define FLDNAME3 job-hdr.i-no
&Global-define FORMAT-3 X(15)
&Global-define SORTBY-3 by job-hdr.i-no
&Global-define IDXNAME3 
&Global-define DESCRIP3 FG Item#
&Global-define FLDNAME4 job-hdr.est-no
&Global-define FORMAT-4 X(5)
&Global-define SORTBY-4 by job-hdr.est-no
&Global-define IDXNAME4 
&Global-define DESCRIP4 Estimate
&Global-define FLDNAME5 job.stat
&Global-define FORMAT-5 X
&Global-define SORTBY-5 by job.stat
&Global-define IDXNAME5 
&Global-define DESCRIP5 Status
&Global-define FLDNAME6 
&Global-define SORTBY-6 
&Global-define IDXNAME6 
&Global-define DESCRIP6 
&Global-define FLDNAME7 
&Global-define SORTBY-7 
&Global-define IDXNAME7 
&Global-define DESCRIP7 
&Global-define FLDNAME8 
&Global-define SORTBY-8 
&Global-define IDXNAME8 
&Global-define DESCRIP8 
&Global-define FLDNAME9 
&Global-define SORTBY-9 
&Global-define IDXNAME9 
&Global-define DESCRIP9 
&Global-define FLDNAME10 
&Global-define SORTBY-10 
&Global-define IDXNAME10 
&Global-define DESCRIP10 
&Global-define FLDNAME11 
&Global-define SORTBY-11 
&Global-define IDXNAME11 
&Global-define DESCRIP11 
&Global-define FLDNAME12 
&Global-define SORTBY-12 
&Global-define IDXNAME12 
&Global-define DESCRIP12 
&Global-define FLDNAME13 
&Global-define SORTBY-13 
&Global-define IDXNAME13 
&Global-define DESCRIP13 
&Global-define ENHANCE yes
