/* ar-cashl.i
"ASI" "ar-cashl"
"inv-no" "Invoice#" ""
"inv-date" "Inv Date" ""
"check-no" "Check" ""
"cust-no" "Customer" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"" "" ""
"yes" "no" "no"
"no"
*/

&Global-define FLDNAME1 ar-cashl.inv-no
&Global-define DATATYP1 INTEGER
&Global-define FORMAT-1 >>>>>9
&Global-define SORTBY-1 BY ar-cashl.inv-no
&Global-define IDXNAME1 
&Global-define DESCRIP1 Invoice#
&Global-define FLDNAME2 ar-cashl.inv-date
&Global-define DATATYP2 DATE
&Global-define FORMAT-2 99/99/9999
&Global-define SORTBY-2 BY ar-cashl.inv-date BY ar-cashl.inv-no
&Global-define IDXNAME2 
&Global-define DESCRIP2 Inv Date
&Global-define FLDNAME3 ar-cashl.check-no
&Global-define FORMAT-3 X(8)
&Global-define SORTBY-3 BY ar-cashl.check-no BY ar-cashl.inv-no
&Global-define IDXNAME3 
&Global-define DESCRIP3 Check
&Global-define FLDNAME4 ar-cashl.cust-no
&Global-define FORMAT-4 X(8)
&Global-define SORTBY-4 BY ar-cashl.cust-no BY ar-cashl.inv-no
&Global-define IDXNAME4 
&Global-define DESCRIP4 Customer
&Global-define FLDNAME5 
&Global-define SORTBY-5 
&Global-define IDXNAME5 
&Global-define DESCRIP5 
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
