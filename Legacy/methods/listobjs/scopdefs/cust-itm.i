/* cust-itm.i
"cust-itm"
"rfq"
"cust-itm"
"cust-no"
"Customer"
"begin_cust-no"
"yes"
"end_cust-no"
"yes"
"i-no"
"FG Item #"
"begin_i-no"
"yes"
"end_i-no"
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
"rd-print"
"yes"
"rd-print-lot"
"yes"
"rd-tot-rel"
"yes"
"ldt-as-of"
"yes"
"t-include-order"
"yes"
""
"no"
"yes"
"yes"
"no"
"no"
"no"
*/

&{&DEFINETYPE}-define TABLENAME cust-itm
&{&DEFINETYPE}-define DBFIELD1 cust-itm.cust-no
&{&DEFINETYPE}-define BEGINFLD1 begin_cust-no
&{&DEFINETYPE}-define BEGIN-DEFAULT-1 yes
&{&DEFINETYPE}-define ENDFLD1 end_cust-no
&{&DEFINETYPE}-define END-DEFAULT-1 yes
&{&DEFINETYPE}-define DBFIELD2 cust-itm.i-no
&{&DEFINETYPE}-define BEGINFLD2 begin_i-no
&{&DEFINETYPE}-define BEGIN-DEFAULT-2 yes
&{&DEFINETYPE}-define ENDFLD2 end_i-no
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
&{&DEFINETYPE}-define LISTORDER Customer,FG Item #
&{&DEFINETYPE}-define ADDFLD-1 rd-print
&{&DEFINETYPE}-define ADD-DEFAULT-1 yes
&{&DEFINETYPE}-define ADDFLD-2 rd-print-lot
&{&DEFINETYPE}-define ADD-DEFAULT-2 yes
&{&DEFINETYPE}-define ADDFLD-3 rd-tot-rel
&{&DEFINETYPE}-define ADD-DEFAULT-3 yes
&{&DEFINETYPE}-define ADDFLD-4 ldt-as-of
&{&DEFINETYPE}-define ADD-DEFAULT-4 yes
&{&DEFINETYPE}-define ADDFLD-5 t-include-order
&{&DEFINETYPE}-define ADD-DEFAULT-5 yes
&{&DEFINETYPE}-define ADDFLD-6 
&{&DEFINETYPE}-define ADD-DEFAULT-6 no
&{&DEFINETYPE}-define DISPLAYFLDS {&ADDFLD-1} {&ADDFLD-2} {&ADDFLD-3} {&ADDFLD-4} {&ADDFLD-5} {&ADDFLD-6}
&{&DEFINETYPE}-define SHOWNOTES yes
&{&DEFINETYPE}-define SHOWMISCFLDS yes
&{&DEFINETYPE}-define SHOWADDRESSES no
&{&DEFINETYPE}-define SHOWPHONES no
&{&DEFINETYPE}-define SAVENAME cust-itm
&{&DEFINETYPE}-define QUERYDEFAULT no
