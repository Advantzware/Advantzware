/* rm-ibtag.i
"rm-ibtag"
"ASI"
"item"
"i-no"
"Item No"
"begin_i-no"
"yes"
"end_i-no"
"yes"
"loc"
"Primary Warehouse"
"begin_whse"
"yes"
"end_whse"
"yes"
"procat"
"Category"
"begin_procat"
"yes"
"end_procat"
"yes"
"mat-type"
"Mat'l Type"
"begin_mtype"
"yes"
"end_mtype"
"yes"
?
""
""
"no"
""
"no"
"t-inc-bal"
"yes"
"t-subtot"
"yes"
"t-gtot"
"yes"
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

&{&DEFINETYPE}-define TABLENAME item
&{&DEFINETYPE}-define DBFIELD1 item.i-no
&{&DEFINETYPE}-define BEGINFLD1 begin_i-no
&{&DEFINETYPE}-define BEGIN-DEFAULT-1 yes
&{&DEFINETYPE}-define ENDFLD1 end_i-no
&{&DEFINETYPE}-define END-DEFAULT-1 yes
&{&DEFINETYPE}-define DBFIELD2 item.loc
&{&DEFINETYPE}-define BEGINFLD2 begin_whse
&{&DEFINETYPE}-define BEGIN-DEFAULT-2 yes
&{&DEFINETYPE}-define ENDFLD2 end_whse
&{&DEFINETYPE}-define END-DEFAULT-2 yes
&{&DEFINETYPE}-define DBFIELD3 item.procat
&{&DEFINETYPE}-define BEGINFLD3 begin_procat
&{&DEFINETYPE}-define BEGIN-DEFAULT-3 yes
&{&DEFINETYPE}-define ENDFLD3 end_procat
&{&DEFINETYPE}-define END-DEFAULT-3 yes
&{&DEFINETYPE}-define DBFIELD4 item.mat-type
&{&DEFINETYPE}-define BEGINFLD4 begin_mtype
&{&DEFINETYPE}-define BEGIN-DEFAULT-4 yes
&{&DEFINETYPE}-define ENDFLD4 end_mtype
&{&DEFINETYPE}-define END-DEFAULT-4 yes
&{&DEFINETYPE}-define DBFIELD5 
&{&DEFINETYPE}-define BEGINFLD5 
&{&DEFINETYPE}-define BEGIN-DEFAULT-5 no
&{&DEFINETYPE}-define ENDFLD5 
&{&DEFINETYPE}-define END-DEFAULT-5 no
&{&DEFINETYPE}-define LISTORDER Item No,Primary Warehouse,Category,Mat'l Type
&{&DEFINETYPE}-define ADDFLD-1 t-inc-bal
&{&DEFINETYPE}-define ADD-DEFAULT-1 yes
&{&DEFINETYPE}-define ADDFLD-2 t-subtot
&{&DEFINETYPE}-define ADD-DEFAULT-2 yes
&{&DEFINETYPE}-define ADDFLD-3 t-gtot
&{&DEFINETYPE}-define ADD-DEFAULT-3 yes
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
&{&DEFINETYPE}-define SAVENAME rm-ibtag
&{&DEFINETYPE}-define QUERYDEFAULT no
