/* contact.i
"EMPTRACK" "contact"
"cust-no" "Cust#" ""
"last-name" "Last Name" ""
"ship-id" "Shipto" ""
"type" "Type" ""
"cust-name" "Company" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"yes" "yes" "no"
"yes"
*/

&Global-define FLDNAME1 contact.cust-no
&Global-define FORMAT-1 X(8)
&Global-define SORTBY-1 BY {&FLDNAME1}
&Global-define IDXNAME1 
&Global-define DESCRIP1 Cust#
&Global-define FLDNAME2 contact.last-name
&Global-define FORMAT-2 X(30)
&Global-define SORTBY-2 BY {&FLDNAME2} {&SORTBY-1}
&Global-define IDXNAME2 
&Global-define DESCRIP2 Last Name
&Global-define FLDNAME3 contact.ship-id
&Global-define FORMAT-3 X(8)
&Global-define SORTBY-3 BY {&FLDNAME3} {&SORTBY-2}
&Global-define IDXNAME3 
&Global-define DESCRIP3 Shipto
&Global-define FLDNAME4 contact.type
&Global-define FORMAT-4 X(3)
&Global-define SORTBY-4 BY {&FLDNAME4} {&SORTBY-3}
&Global-define IDXNAME4 
&Global-define DESCRIP4 Type
&Global-define FLDNAME5 contact.cust-name
&Global-define FORMAT-5 X(30)
&Global-define SORTBY-5 BY {&FLDNAME5} {&SORTBY-4}
&Global-define IDXNAME5 
&Global-define DESCRIP5 Company
&Global-define FLDNAME6 
&Global-define SORTBY-6 
&Global-define IDXNAME6 
&Global-define DESCRIP6 
&Global-define FLDNAME7 
&Global-define SORTBY-7 
&Global-define IDXNAME7 
&Global-define DESCRIP7 
&Global-define FLDNAME8 
&Global-define SORTBY-8 
&Global-define IDXNAME8 
&Global-define DESCRIP8 
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
