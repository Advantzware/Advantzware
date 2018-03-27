/* itemfg.i
"asi" "itemfg"
"i-no" "Item No" ""
"i-name" "Name" ""
"cad-no" "Cad#" ""
"est-no" "Est #" ""
"i-code" "Stock/Custom" ""
"procat" "Cat" ""
"spc-no" "Spc#" ""
"style" "Style" ""
"stocked" "Stocked?" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"yes" "yes" "no"
"yes"
*/

&Global-define FLDNAME1 itemfg.i-no
&Global-define FORMAT-1 X(15)
&Global-define SORTBY-1 BY {&FLDNAME1}
&Global-define IDXNAME1 
&Global-define DESCRIP1 Item#
&Global-define FLDNAME2 itemfg.i-name
&Global-define FORMAT-2 X(30)
&Global-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Global-define IDXNAME2 
&Global-define DESCRIP2 Name
&Global-define FLDNAME3 itemfg.cad-no
&Global-define FORMAT-3 X(15)
&Global-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-1}
&Global-define IDXNAME3 
&Global-define DESCRIP3 CAD#
&Global-define FLDNAME4 itemfg.est-no
&Global-define FORMAT-4 X(5)
&Global-define SORTBY-4 BY {&FLDNAME4} {&SORTBY-1}
&Global-define IDXNAME4 
&Global-define DESCRIP4 Est#
&Global-define FLDNAME5 itemfg.i-code
&Global-define FORMAT-5 X
&Global-define SORTBY-5 BY {&FLDNAME5}  {&SORTBY-1}
&Global-define IDXNAME5 
&Global-define DESCRIP5 Stock/Custom
&Global-define FLDNAME6 itemfg.procat
&Global-define FORMAT-6 X(5)
&Global-define SORTBY-6 BY {&FLDNAME6}  {&SORTBY-1}
&Global-define IDXNAME6 
&Global-define DESCRIP6 Cat
&Global-define FLDNAME7 itemfg.spc-no
&Global-define FORMAT-7 X(15)
&Global-define SORTBY-7 BY {&FLDNAME7}  {&SORTBY-1}
&Global-define IDXNAME7 
&Global-define DESCRIP7 SPC#
&Global-define FLDNAME8 itemfg.style
&Global-define FORMAT-8 X(6)
&Global-define SORTBY-8 BY {&FLDNAME8}  {&SORTBY-1}
&Global-define IDXNAME8 
&Global-define DESCRIP8 Style
&Global-define FLDNAME9 itemfg.stocked
&Global-define DATATYP9 LOGICAL
&Global-define FORMAT-9 YES/NO
&Global-define SORTBY-9 BY {&FLDNAME9} {&SORTBY-1}
&Global-define IDXNAME9 
&Global-define DESCRIP9 Stocked?
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
