/* post.i
"post"
"EMPTRACK"
"machtran"
"machine"
"Machine"
"begin_machine"
"yes"
"end_machine"
"yes"
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
"begin_date"
"yes"
"end_date"
"yes"
"begin_shift"
"yes"
"end_shift"
"yes"
"post"
"no"
""
"no"
"no"
"no"
"no"
"no"
"no"
*/

&{&DEFINETYPE}-define TABLENAME machtran
&{&DEFINETYPE}-define DBFIELD1 machtran.machine
&{&DEFINETYPE}-define BEGINFLD1 begin_machine
&{&DEFINETYPE}-define BEGIN-DEFAULT-1 yes
&{&DEFINETYPE}-define ENDFLD1 end_machine
&{&DEFINETYPE}-define END-DEFAULT-1 yes
&{&DEFINETYPE}-define DBFIELD2 
&{&DEFINETYPE}-define BEGINFLD2 
&{&DEFINETYPE}-define BEGIN-DEFAULT-2 no
&{&DEFINETYPE}-define ENDFLD2 
&{&DEFINETYPE}-define END-DEFAULT-2 no
&{&DEFINETYPE}-define DBFIELD3 
&{&DEFINETYPE}-define BEGINFLD3 
&{&DEFINETYPE}-define BEGIN-DEFAULT-3 no
&{&DEFINETYPE}-define ENDFLD3 
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
&{&DEFINETYPE}-define LISTORDER Machine
&{&DEFINETYPE}-define ADDFLD-1 begin_date
&{&DEFINETYPE}-define ADD-DEFAULT-1 yes
&{&DEFINETYPE}-define ADDFLD-2 end_date
&{&DEFINETYPE}-define ADD-DEFAULT-2 yes
&{&DEFINETYPE}-define ADDFLD-3 begin_shift
&{&DEFINETYPE}-define ADD-DEFAULT-3 yes
&{&DEFINETYPE}-define ADDFLD-4 end_shift
&{&DEFINETYPE}-define ADD-DEFAULT-4 yes
&{&DEFINETYPE}-define ADDFLD-5 post
&{&DEFINETYPE}-define ADD-DEFAULT-5 no
&{&DEFINETYPE}-define ADDFLD-6 
&{&DEFINETYPE}-define ADD-DEFAULT-6 no
&{&DEFINETYPE}-define DISPLAYFLDS {&ADDFLD-1} {&ADDFLD-2} {&ADDFLD-3} {&ADDFLD-4} {&ADDFLD-5} {&ADDFLD-6}
&{&DEFINETYPE}-define SHOWNOTES no
&{&DEFINETYPE}-define SHOWMISCFLDS no
&{&DEFINETYPE}-define SHOWADDRESSES no
&{&DEFINETYPE}-define SHOWPHONES no
&{&DEFINETYPE}-define SAVENAME post
&{&DEFINETYPE}-define QUERYDEFAULT no
