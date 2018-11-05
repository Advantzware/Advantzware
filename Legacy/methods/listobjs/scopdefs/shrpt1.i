/* shrpt1.i
"shrpt1"
"ASI"
"item"
"procat"
"Category"
"begin_procat"
"yes"
"end_procat"
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
"rd-mat-type"
"yes"
"is-real-mat"
"yes"
"is-est-mat"
"yes"
"is-detail"
"yes"
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

&{&DEFINETYPE}-define TABLENAME item
&{&DEFINETYPE}-define DBFIELD1 item.procat
&{&DEFINETYPE}-define BEGINFLD1 begin_procat
&{&DEFINETYPE}-define BEGIN-DEFAULT-1 yes
&{&DEFINETYPE}-define ENDFLD1 end_procat
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
&{&DEFINETYPE}-define LISTORDER Category
&{&DEFINETYPE}-define ADDFLD-1 rd-mat-type
&{&DEFINETYPE}-define ADD-DEFAULT-1 yes
&{&DEFINETYPE}-define ADDFLD-2 is-real-mat
&{&DEFINETYPE}-define ADD-DEFAULT-2 yes
&{&DEFINETYPE}-define ADDFLD-3 is-est-mat
&{&DEFINETYPE}-define ADD-DEFAULT-3 yes
&{&DEFINETYPE}-define ADDFLD-4 is-detail
&{&DEFINETYPE}-define ADD-DEFAULT-4 yes
&{&DEFINETYPE}-define ADDFLD-5 
&{&DEFINETYPE}-define ADD-DEFAULT-5 no
&{&DEFINETYPE}-define ADDFLD-6 
&{&DEFINETYPE}-define ADD-DEFAULT-6 no
&{&DEFINETYPE}-define DISPLAYFLDS {&ADDFLD-1} {&ADDFLD-2} {&ADDFLD-3} {&ADDFLD-4} {&ADDFLD-5} {&ADDFLD-6}
&{&DEFINETYPE}-define SHOWNOTES yes
&{&DEFINETYPE}-define SHOWMISCFLDS yes
&{&DEFINETYPE}-define SHOWADDRESSES no
&{&DEFINETYPE}-define SHOWPHONES no
&{&DEFINETYPE}-define SAVENAME shrpt1
&{&DEFINETYPE}-define QUERYDEFAULT yes
