/* rm-rhist.i
"rm-rhist"
"ASI"
"item"
"beg-date"
"Beg Bal Date"
"begin_date"
"yes"
"end_date"
"yes"
"i-no"
"Item No"
"begin_i-no"
"yes"
"end_i-no"
"yes"
"vend-no"
"Vendor Number"
"begin_ven"
"yes"
"end_ven"
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

&{&DEFINETYPE}-define TABLENAME item
&{&DEFINETYPE}-define DATATYP1 DATE
&{&DEFINETYPE}-define DBFIELD1 item.beg-date
&{&DEFINETYPE}-define BEGINFLD1 begin_date
&{&DEFINETYPE}-define BEGIN-DEFAULT-1 yes
&{&DEFINETYPE}-define ENDFLD1 end_date
&{&DEFINETYPE}-define END-DEFAULT-1 yes
&{&DEFINETYPE}-define DBFIELD2 item.i-no
&{&DEFINETYPE}-define BEGINFLD2 begin_i-no
&{&DEFINETYPE}-define BEGIN-DEFAULT-2 yes
&{&DEFINETYPE}-define ENDFLD2 end_i-no
&{&DEFINETYPE}-define END-DEFAULT-2 yes
&{&DEFINETYPE}-define DBFIELD3 item.vend-no
&{&DEFINETYPE}-define BEGINFLD3 begin_ven
&{&DEFINETYPE}-define BEGIN-DEFAULT-3 yes
&{&DEFINETYPE}-define ENDFLD3 end_ven
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
&{&DEFINETYPE}-define LISTORDER Beg Bal Date,Item No,Vendor Number
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
&{&DEFINETYPE}-define SAVENAME rm-rhist
&{&DEFINETYPE}-define QUERYDEFAULT yes
