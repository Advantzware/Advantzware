/* est.i  /*  Manually changed for w-est.w, w-estc.w  YSK */
"ASI" "est"
"est-no" "Estimate #" ""
"eb.cust-no" "Cust#" ""
"eb.part-no" "Cust Part#" ""
"eb.stock-no" "FG Item#" ""
"eb.style" "Style" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"yes" "no" "no"
"yes"
*/

&Global-define FLDNAME1 est.est-no
&Global-define FORMAT-1 X(5)
&Global-define SORTBY-1 BY int({&FLDNAME1}) descending
&Global-define IDXNAME1 
&Global-define DESCRIP1 Est#
&Global-define FLDNAME2 eb.cust-no
&Global-define FORMAT-2 X(8)
&Global-define SORTBY-2 by {&fldname2} by eb.style by eb.len by eb.wid by eb.dep
&Global-define IDXNAME2 
&Global-define DESCRIP2 Cust#
&Global-define FLDNAME3 eb.part-no
&Global-define FORMAT-3 X(15)
&Global-define SORTBY-3 by {&FLDNAME3}
&Global-define IDXNAME3 
&Global-define DESCRIP3 Cust Part#
&Global-define FLDNAME4 eb.stock-no
&Global-define FORMAT-4 X(15)
&Global-define SORTBY-4 by {&fldname4}
&Global-define IDXNAME4 
&Global-define DESCRIP4 FG Item#
&Global-define FLDNAME5 eb.style
&Global-define FORMAT-5 X(5)
&Global-define SORTBY-5 by {&fldname5}
&Global-define IDXNAME5 
&Global-define DESCRIP5 Style
&Global-define FLDNAME6 string(eb.len)  
&Global-define DESCRIP6 Length
&Global-define FORMAT-6 x(8)
&Global-define SORTBY-6 by (eb.len)
&Global-define FLDNAME7 eb.die-no
&Global-define FORMAT-7 x(15)
&Global-define SORTBY-7 BY {&fldname7}
&Global-define IDXNAME7 
&Global-define DESCRIP7 Die#
&Global-define FLDNAME8 eb.plate-no
&Global-define FORMAT-8 x(15)
&Global-define SORTBY-8 BY {&fldname8}
&Global-define IDXNAME8 
&Global-define DESCRIP8 Plate#
&Global-define FLDNAME9 
&Global-define SORTBY-9 
&Global-define IDXNAME9 
&Global-define DESCRIP9 
&Global-define FLDNAME10 
&Global-define SORTBY-10 
&Global-define IDXNAME10 
&Global-define DESCRIP10 
&Global-define FLDNAME11 
&Global-define SORTBY-11 
&Global-define IDXNAME11 
&Global-define DESCRIP11 
&Global-define FLDNAME12 
&Global-define SORTBY-12 
&Global-define IDXNAME12 
&Global-define DESCRIP12 
&Global-define FLDNAME13 
&Global-define SORTBY-13 
&Global-define IDXNAME13 
&Global-define DESCRIP13 
&Global-define ENHANCE yes
