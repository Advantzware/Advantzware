/* oe-ord.i
"ASI" "oe-ord"
"ord-no" "Order#" ""
"ord-date" "Date" ""
"cust-no" "Cust. #" ""
"est-no" "Estimate #" ""
"job-no" "Job#" ""
"i-no" "Item#" ""
"stat" "Status" ""
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

&Global-define FLDNAME1 oe-ord.ord-no
&Global-define DATATYP1 INTEGER
&Global-define FORMAT-1 >>>>>9
&Global-define SORTBY-1 BY {&FLDNAME1} descending
&Global-define IDXNAME1 
&Global-define DESCRIP1 Order#
&Global-define FLDNAME2 oe-ord.ord-date
&Global-define DATATYP2 DATE
&Global-define FORMAT-2 99/99/9999
&Global-define SORTBY-2 BY {&FLDNAME2}
&Global-define IDXNAME2 
&Global-define DESCRIP2 Date
&Global-define FLDNAME3 oe-ord.cust-no
&Global-define FORMAT-3 X(8)
&Global-define SORTBY-3 BY {&FLDNAME3}
&Global-define IDXNAME3 
&Global-define DESCRIP3 Cust. #
&Global-define FLDNAME4 oe-ordl.est-no
&Global-define FORMAT-4 X(5)
&Global-define SORTBY-4 BY {&FLDNAME4}
&Global-define IDXNAME4 
&Global-define DESCRIP4 Estimate #
&Global-define FLDNAME5 oe-ordl.job-no
&Global-define SORTBY-5 by {&fldname5}
&Global-define FORMAT-5 X(6)
&Global-define IDXNAME5 
&Global-define DESCRIP5 Job#
&Global-define FLDNAME6 oe-ordl.i-no
&Global-define FORMAT-6 X(10)
&Global-define SORTBY-6 by {&fldname6}
&Global-define IDXNAME6 
&Global-define DESCRIP6 Item#
&Global-define FLDNAME7 oe-ord.stat
&Global-define FORMAT-7 X 
&Global-define SORTBY-7 BY {&fldname7}
&Global-define IDXNAME7 
&Global-define DESCRIP7 Status
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
