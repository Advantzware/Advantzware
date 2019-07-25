/* employee.i
"employee"
"EMPTRACK"
"employee"
"employee"
"Emp ID"
"begin_employee"
"yes"
"end_employee"
"yes"
"last_name"
"Last Name"
"begin_employee_last_name"
"yes"
"end_employee_last_name"
"yes"
"soc_sec"
"Social Security #"
"begin_employee_soc_sec"
"yes"
"end_employee_soc_sec"
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
"selected-company"
"no"
"show-rates"
"no"
"show-login-logout"
"no"
"show-machines"
"no"
"show-emp-notes"
"no"
""
"no"
"yes"
"yes"
"yes"
"yes"
"yes"
*/

&{&DEFINETYPE}-define TABLENAME employee
&{&DEFINETYPE}-define DBFIELD1 employee.employee
&{&DEFINETYPE}-define BEGINFLD1 begin_employee
&{&DEFINETYPE}-define BEGIN-DEFAULT-1 yes
&{&DEFINETYPE}-define ENDFLD1 end_employee
&{&DEFINETYPE}-define END-DEFAULT-1 yes
&{&DEFINETYPE}-define DBFIELD2 employee.last_name
&{&DEFINETYPE}-define BEGINFLD2 begin_employee_last_name
&{&DEFINETYPE}-define BEGIN-DEFAULT-2 yes
&{&DEFINETYPE}-define ENDFLD2 end_employee_last_name
&{&DEFINETYPE}-define END-DEFAULT-2 yes
&{&DEFINETYPE}-define DBFIELD3 employee.soc_sec
&{&DEFINETYPE}-define BEGINFLD3 begin_employee_soc_sec
&{&DEFINETYPE}-define BEGIN-DEFAULT-3 yes
&{&DEFINETYPE}-define ENDFLD3 end_employee_soc_sec
&{&DEFINETYPE}-define END-DEFAULT-3 yes
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
&{&DEFINETYPE}-define LISTORDER Emp ID,Last Name,Social Security #
&{&DEFINETYPE}-define ADDFLD-1 selected-company
&{&DEFINETYPE}-define ADD-DEFAULT-1 no
&{&DEFINETYPE}-define ADDFLD-2 show-rates
&{&DEFINETYPE}-define ADD-DEFAULT-2 no
&{&DEFINETYPE}-define ADDFLD-3 show-login-logout
&{&DEFINETYPE}-define ADD-DEFAULT-3 no
&{&DEFINETYPE}-define ADDFLD-4 show-machines
&{&DEFINETYPE}-define ADD-DEFAULT-4 no
&{&DEFINETYPE}-define ADDFLD-5 show-emp-notes
&{&DEFINETYPE}-define ADD-DEFAULT-5 no
&{&DEFINETYPE}-define ADDFLD-6 
&{&DEFINETYPE}-define ADD-DEFAULT-6 no
&{&DEFINETYPE}-define DISPLAYFLDS {&ADDFLD-1} {&ADDFLD-2} {&ADDFLD-3} {&ADDFLD-4} {&ADDFLD-5} {&ADDFLD-6}
&{&DEFINETYPE}-define SHOWNOTES yes
&{&DEFINETYPE}-define SHOWMISCFLDS yes
&{&DEFINETYPE}-define SHOWADDRESSES yes
&{&DEFINETYPE}-define SHOWPHONES yes
&{&DEFINETYPE}-define SAVENAME employee
&{&DEFINETYPE}-define QUERYDEFAULT yes
