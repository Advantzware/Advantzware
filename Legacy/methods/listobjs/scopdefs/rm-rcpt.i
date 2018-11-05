/* rm-rcpt.i
"rm-rcpt"
"ASI"
"rm-rcpt"
"po-no"
"PO#"
"begin_po-no"
"yes"
"end_po-no"
"yes"
"i-no"
"Item No"
"begin_rm-rcpt_i-no"
"no"
"end_rm-rcpt_i-no"
"no"
"job-no"
"Job#"
"begin_rm-rcpt_job-no"
"no"
"end_rm-rcpt_job-no"
"no"
?
""
""
"no"
""
"no"
?
""
""
"no"
""
"no"
""
"no"
""
"no"
""
"no"
""
"no"
""
"no"
""
"no"
"yes"
"yes"
"no"
"no"
"yes"
*/

&{&DEFINETYPE}-define TABLENAME rm-rcpt
&{&DEFINETYPE}-define DBFIELD1 rm-rcpt.po-no
&{&DEFINETYPE}-define BEGINFLD1 begin_po-no
&{&DEFINETYPE}-define BEGIN-DEFAULT-1 yes
&{&DEFINETYPE}-define ENDFLD1 end_po-no
&{&DEFINETYPE}-define END-DEFAULT-1 yes
&{&DEFINETYPE}-define DBFIELD2 rm-rcpt.i-no
&{&DEFINETYPE}-define BEGINFLD2 begin_rm-rcpt_i-no
&{&DEFINETYPE}-define BEGIN-DEFAULT-2 no
&{&DEFINETYPE}-define ENDFLD2 end_rm-rcpt_i-no
&{&DEFINETYPE}-define END-DEFAULT-2 no
&{&DEFINETYPE}-define DBFIELD3 rm-rcpt.job-no
&{&DEFINETYPE}-define BEGINFLD3 begin_rm-rcpt_job-no
&{&DEFINETYPE}-define BEGIN-DEFAULT-3 no
&{&DEFINETYPE}-define ENDFLD3 end_rm-rcpt_job-no
&{&DEFINETYPE}-define END-DEFAULT-3 no
&{&DEFINETYPE}-define DBFIELD4 
&{&DEFINETYPE}-define BEGINFLD4 
&{&DEFINETYPE}-define BEGIN-DEFAULT-4 no
&{&DEFINETYPE}-define ENDFLD4 
&{&DEFINETYPE}-define END-DEFAULT-4 no
&{&DEFINETYPE}-define DBFIELD5 
&{&DEFINETYPE}-define BEGINFLD5 
&{&DEFINETYPE}-define BEGIN-DEFAULT-5 no
&{&DEFINETYPE}-define ENDFLD5 
&{&DEFINETYPE}-define END-DEFAULT-5 no
&{&DEFINETYPE}-define LISTORDER PO#,Item No,Job#
&{&DEFINETYPE}-define ADDFLD-1 
&{&DEFINETYPE}-define ADD-DEFAULT-1 no
&{&DEFINETYPE}-define ADDFLD-2 
&{&DEFINETYPE}-define ADD-DEFAULT-2 no
&{&DEFINETYPE}-define ADDFLD-3 
&{&DEFINETYPE}-define ADD-DEFAULT-3 no
&{&DEFINETYPE}-define ADDFLD-4 
&{&DEFINETYPE}-define ADD-DEFAULT-4 no
&{&DEFINETYPE}-define ADDFLD-5 
&{&DEFINETYPE}-define ADD-DEFAULT-5 no
&{&DEFINETYPE}-define ADDFLD-6 
&{&DEFINETYPE}-define ADD-DEFAULT-6 no
&{&DEFINETYPE}-define DISPLAYFLDS {&ADDFLD-1} {&ADDFLD-2} {&ADDFLD-3} {&ADDFLD-4} {&ADDFLD-5} {&ADDFLD-6}
&{&DEFINETYPE}-define SHOWNOTES yes
&{&DEFINETYPE}-define SHOWMISCFLDS yes
&{&DEFINETYPE}-define SHOWADDRESSES no
&{&DEFINETYPE}-define SHOWPHONES no
&{&DEFINETYPE}-define SAVENAME rm-rcpt
&{&DEFINETYPE}-define QUERYDEFAULT yes
