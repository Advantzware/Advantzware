/* emplogin.i
"emplogin"
"EMPTRACK"
"emplogin"
"employee"
"Emp ID"
"begin_employee"
"yes"
"end_employee"
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
"begin_end_date"
"yes"
"end_end_date"
"yes"
"lv-labor-only"
"no"
"lv-note-only"
"no"
"begin_note-date"
"no"
"end_note-date"
"no"
"no"
"no"
"no"
"no"
"yes"
*/

&{&DEFINETYPE}-define TABLENAME emplogin
&{&DEFINETYPE}-define DBFIELD1 emplogin.employee
&{&DEFINETYPE}-define BEGINFLD1 begin_employee
&{&DEFINETYPE}-define BEGIN-DEFAULT-1 yes
&{&DEFINETYPE}-define ENDFLD1 end_employee
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
&{&DEFINETYPE}-define LISTORDER Emp ID
&{&DEFINETYPE}-define ADDFLD-1 begin_end_date
&{&DEFINETYPE}-define ADD-DEFAULT-1 yes
&{&DEFINETYPE}-define ADDFLD-2 end_end_date
&{&DEFINETYPE}-define ADD-DEFAULT-2 yes
&{&DEFINETYPE}-define ADDFLD-3 lv-labor-only
&{&DEFINETYPE}-define ADD-DEFAULT-3 no
&{&DEFINETYPE}-define ADDFLD-4 lv-note-only
&{&DEFINETYPE}-define ADD-DEFAULT-4 no
&{&DEFINETYPE}-define ADDFLD-5 begin_note-date
&{&DEFINETYPE}-define ADD-DEFAULT-5 no
&{&DEFINETYPE}-define ADDFLD-6 end_note-date
&{&DEFINETYPE}-define ADD-DEFAULT-6 no
&{&DEFINETYPE}-define DISPLAYFLDS {&ADDFLD-1} {&ADDFLD-2} {&ADDFLD-3} {&ADDFLD-4} {&ADDFLD-5} {&ADDFLD-6}
&{&DEFINETYPE}-define SHOWNOTES no
&{&DEFINETYPE}-define SHOWMISCFLDS no
&{&DEFINETYPE}-define SHOWADDRESSES no
&{&DEFINETYPE}-define SHOWPHONES no
&{&DEFINETYPE}-define SAVENAME emplogin
&{&DEFINETYPE}-define QUERYDEFAULT yes
