/* oe-prmtx.i
"oe-prmtx"
"ASI"
"oe-prmtx"
"company"
"Company"
"begin_company"
"yes"
"end_company"
"yes"
"cust-no"
"Customer"
"begin_oe-prmtx_cust-no"
"no"
"end_oe-prmtx_cust-no"
"no"
"custype"
"Type"
"begin_oe-prmtx_custype"
"no"
"end_oe-prmtx_custype"
"no"
"i-no"
"Item No"
"begin_oe-prmtx_i-no"
"no"
"end_oe-prmtx_i-no"
"no"
"meth"
"Price Basis"
"begin_oe-prmtx_meth"
"no"
"end_oe-prmtx_meth"
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

&{&DEFINETYPE}-define TABLENAME oe-prmtx
&{&DEFINETYPE}-define DBFIELD1 oe-prmtx.company
&{&DEFINETYPE}-define BEGINFLD1 begin_company
&{&DEFINETYPE}-define BEGIN-DEFAULT-1 yes
&{&DEFINETYPE}-define ENDFLD1 end_company
&{&DEFINETYPE}-define END-DEFAULT-1 yes
&{&DEFINETYPE}-define DBFIELD2 oe-prmtx.cust-no
&{&DEFINETYPE}-define BEGINFLD2 begin_oe-prmtx_cust-no
&{&DEFINETYPE}-define BEGIN-DEFAULT-2 no
&{&DEFINETYPE}-define ENDFLD2 end_oe-prmtx_cust-no
&{&DEFINETYPE}-define END-DEFAULT-2 no
&{&DEFINETYPE}-define DBFIELD3 oe-prmtx.custype
&{&DEFINETYPE}-define BEGINFLD3 begin_oe-prmtx_custype
&{&DEFINETYPE}-define BEGIN-DEFAULT-3 no
&{&DEFINETYPE}-define ENDFLD3 end_oe-prmtx_custype
&{&DEFINETYPE}-define END-DEFAULT-3 no
&{&DEFINETYPE}-define DBFIELD4 oe-prmtx.i-no
&{&DEFINETYPE}-define BEGINFLD4 begin_oe-prmtx_i-no
&{&DEFINETYPE}-define BEGIN-DEFAULT-4 no
&{&DEFINETYPE}-define ENDFLD4 end_oe-prmtx_i-no
&{&DEFINETYPE}-define END-DEFAULT-4 no
&{&DEFINETYPE}-define DATATYP5 LOGICAL
&{&DEFINETYPE}-define DBFIELD5 oe-prmtx.meth
&{&DEFINETYPE}-define BEGINFLD5 begin_oe-prmtx_meth
&{&DEFINETYPE}-define BEGIN-DEFAULT-5 no
&{&DEFINETYPE}-define ENDFLD5 end_oe-prmtx_meth
&{&DEFINETYPE}-define END-DEFAULT-5 no
&{&DEFINETYPE}-define LISTORDER Company,Customer,Type,Item No,Price Basis
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
&{&DEFINETYPE}-define SAVENAME oe-prmtx
&{&DEFINETYPE}-define QUERYDEFAULT yes
