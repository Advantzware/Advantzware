/* eb.i
"eb"
"ASI"
"eb"
"cust-no"
"Cust. #"
"begin_cust-no"
"yes"
"end_cust-no"
"yes"
"style"
"Style Code"
"begin_eb_style"
"yes"
"end_eb_style"
"yes"
"flute"
"Flute"
"begin_eb_flute"
"yes"
"end_eb_flute"
"yes"
"test"
"Test"
"begin_eb_test"
"yes"
"end_eb_test"
"yes"
?
""
""
"no"
""
"no"
"rd-industry"
"yes"
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
"no"
"no"
"no"
"no"
"no"
*/

&{&DEFINETYPE}-define TABLENAME eb
&{&DEFINETYPE}-define DBFIELD1 eb.cust-no
&{&DEFINETYPE}-define BEGINFLD1 begin_cust-no
&{&DEFINETYPE}-define BEGIN-DEFAULT-1 yes
&{&DEFINETYPE}-define ENDFLD1 end_cust-no
&{&DEFINETYPE}-define END-DEFAULT-1 yes
&{&DEFINETYPE}-define DBFIELD2 eb.style
&{&DEFINETYPE}-define BEGINFLD2 begin_eb_style
&{&DEFINETYPE}-define BEGIN-DEFAULT-2 yes
&{&DEFINETYPE}-define ENDFLD2 end_eb_style
&{&DEFINETYPE}-define END-DEFAULT-2 yes
&{&DEFINETYPE}-define DBFIELD3 eb.flute
&{&DEFINETYPE}-define BEGINFLD3 begin_eb_flute
&{&DEFINETYPE}-define BEGIN-DEFAULT-3 yes
&{&DEFINETYPE}-define ENDFLD3 end_eb_flute
&{&DEFINETYPE}-define END-DEFAULT-3 yes
&{&DEFINETYPE}-define DBFIELD4 eb.test
&{&DEFINETYPE}-define BEGINFLD4 begin_eb_test
&{&DEFINETYPE}-define BEGIN-DEFAULT-4 yes
&{&DEFINETYPE}-define ENDFLD4 end_eb_test
&{&DEFINETYPE}-define END-DEFAULT-4 yes
&{&DEFINETYPE}-define DBFIELD5 
&{&DEFINETYPE}-define BEGINFLD5 
&{&DEFINETYPE}-define BEGIN-DEFAULT-5 no
&{&DEFINETYPE}-define ENDFLD5 
&{&DEFINETYPE}-define END-DEFAULT-5 no
&{&DEFINETYPE}-define LISTORDER Cust. #,Style Code,Flute,Test
&{&DEFINETYPE}-define ADDFLD-1 rd-industry
&{&DEFINETYPE}-define ADD-DEFAULT-1 yes
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
&{&DEFINETYPE}-define SHOWNOTES no
&{&DEFINETYPE}-define SHOWMISCFLDS no
&{&DEFINETYPE}-define SHOWADDRESSES no
&{&DEFINETYPE}-define SHOWPHONES no
&{&DEFINETYPE}-define SAVENAME eb
&{&DEFINETYPE}-define QUERYDEFAULT no
