/* reconcile.i  Reconcile Bank Statement
"asi" "attach"
"attach-file" "File Name" ""
"run-application" "Open With" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"yes" "yes" "yes"
"yes"
*/

&Global-define FLDNAME1 attach.attach-file
&Global-define FORMAT-1 x(50)
&Global-define SORTBY-1 BY {&FLDNAME1} BY attach.creat-date BY attach.rec_key
&Global-define IDXNAME1 
&Global-define DESCRIP1 File Name
&Global-define FLDNAME2 attach.run-application
&Global-define FORMAT-2 x(30)
&Global-define SORTBY-2 BY {&FLDNAME2} BY {&FLDNAME1} 
&Global-define IDXNAME2 
&Global-define DESCRIP2 Open With
&Global-define FLDNAME3 
&Global-define FORMAT-3 
&Global-define SORTBY-3 
&Global-define IDXNAME3 
&Global-define DESCRIP3 
&Global-define FLDNAME4 
&Global-define SORTBY-4 
&Global-define IDXNAME4 
&Global-define DESCRIP4 
&Global-define FLDNAME5 
&Global-define SORTBY-5 
&Global-define IDXNAME5 
&Global-define DESCRIP5 
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
