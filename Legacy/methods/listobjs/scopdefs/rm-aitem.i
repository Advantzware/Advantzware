/* rm-aitem.i
"rm-aitem"
"ASI"
"item"
"i-no"
"Item No"
"begin_i-no"
"yes"
"end_i-no"
"yes"
"procat"
"Category"
"begin_item_procat"
"yes"
"end_item_procat"
"yes"
"mat-type"
"Mat'l Type"
"begin_item_mat-type"
"yes"
"end_item_mat-type"
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
&{&DEFINETYPE}-define DBFIELD1 item.i-no
&{&DEFINETYPE}-define BEGINFLD1 begin_i-no
&{&DEFINETYPE}-define BEGIN-DEFAULT-1 yes
&{&DEFINETYPE}-define ENDFLD1 end_i-no
&{&DEFINETYPE}-define END-DEFAULT-1 yes
&{&DEFINETYPE}-define DBFIELD2 item.procat
&{&DEFINETYPE}-define BEGINFLD2 begin_item_procat
&{&DEFINETYPE}-define BEGIN-DEFAULT-2 yes
&{&DEFINETYPE}-define ENDFLD2 end_item_procat
&{&DEFINETYPE}-define END-DEFAULT-2 yes
&{&DEFINETYPE}-define DBFIELD3 item.mat-type
&{&DEFINETYPE}-define BEGINFLD3 begin_item_mat-type
&{&DEFINETYPE}-define BEGIN-DEFAULT-3 yes
&{&DEFINETYPE}-define ENDFLD3 end_item_mat-type
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
&{&DEFINETYPE}-define LISTORDER Item No,Category,Mat'l Type
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
&{&DEFINETYPE}-define SAVENAME rm-aitem
&{&DEFINETYPE}-define QUERYDEFAULT yes
