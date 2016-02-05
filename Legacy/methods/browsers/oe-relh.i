/* oe-relh.i  overrided by YSK to sort by oe-rell with oe-relh 
"ASI" "oe-relh"
"ord-no" "Order#" ""
"po-no" "P.O. Num" ""
"rel-no" "Rel#" ""
"job-no" "Job#" ""
"i-no" "FG Item" ""
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

&Global-define FLDNAME1 oe-rell.ord-no
&Global-define DATATYP1 INTEGER
&Global-define FORMAT-1 >>>>>9
&Global-define SORTBY-1 BY {&FLDNAME1} descending
&Global-define IDXNAME1 
&Global-define DESCRIP1 Order#
&GLOBAL-DEFINE use-browse1 browse-ord-no
&Global-define FLDNAME2 oe-rell.po-no
&Global-define FORMAT-2 X(15)
&Global-define SORTBY-2 BY {&FLDNAME2} descending
&Global-define IDXNAME2 
&Global-define DESCRIP2 P.O. Num
&GLOBAL-DEFINE use-browse2 browse-po-no
&Global-define FLDNAME3 oe-relh.rel-no
&Global-define DATATYP3 INTEGER
&Global-define FORMAT-3 >>9
&Global-define SORTBY-3 BY {&FLDNAME3}
&Global-define IDXNAME3 
&Global-define DESCRIP3 Rel#
&Global-define FLDNAME4 oe-rell.job-no
&Global-define SORTBY-4 by {&FLDNAME4}
&Global-define IDXNAME4 
&Global-define FORMAT-4 x(6)
&Global-define DESCRIP4 Job#
&GLOBAL-DEFINE use-browse4 browse-job-no
&Global-define FLDNAME5 oe-rell.i-no
&Global-define SORTBY-5 by {&FLDNAME5}
&Global-define IDXNAME5
&Global-define FORMAT-5 X(10) 
&Global-define DESCRIP5 FG Item#
&GLOBAL-DEFINE use-browse5 browse-i-no
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
