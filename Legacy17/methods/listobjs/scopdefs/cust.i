/* cust.i
"cust"
"ASI"
"cust"
"cust-no"
"Cust. #"
"begin_cust-no"
"yes"
"end_cust-no"
"yes"
"name"
"Customer Name"
"begin_cust_name"
"yes"
"end_cust_name"
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
"show-shipto"
"no"
"show-soldto"
"no"
"show-totals"
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

&{&DEFINETYPE}-define TABLENAME cust
&{&DEFINETYPE}-define DBFIELD1 cust.cust-no
&{&DEFINETYPE}-define BEGINFLD1 begin_cust-no
&{&DEFINETYPE}-define BEGIN-DEFAULT-1 yes
&{&DEFINETYPE}-define ENDFLD1 end_cust-no
&{&DEFINETYPE}-define END-DEFAULT-1 yes
&{&DEFINETYPE}-define DBFIELD2 cust.name
&{&DEFINETYPE}-define BEGINFLD2 begin_cust_name
&{&DEFINETYPE}-define BEGIN-DEFAULT-2 yes
&{&DEFINETYPE}-define ENDFLD2 end_cust_name
&{&DEFINETYPE}-define END-DEFAULT-2 yes
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
&{&DEFINETYPE}-define LISTORDER Cust. #,Customer Name
&{&DEFINETYPE}-define ADDFLD-1 show-shipto
&{&DEFINETYPE}-define ADD-DEFAULT-1 no
&{&DEFINETYPE}-define ADDFLD-2 show-soldto
&{&DEFINETYPE}-define ADD-DEFAULT-2 no
&{&DEFINETYPE}-define ADDFLD-3 show-totals
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
&{&DEFINETYPE}-define SAVENAME cust
&{&DEFINETYPE}-define QUERYDEFAULT yes
