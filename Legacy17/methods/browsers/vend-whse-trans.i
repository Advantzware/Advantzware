/* vend-whse-trans.i
"asi" "vend-whse-trans"
"r-no" "Seq#" ""
"tag" "Tag#" ""
"rct-date" "Receipt Date" ""
"po-no" "PO#" ""
"i-no" "Item No" ""
"job-no" "Job#" ""
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

&Global-define FLDNAME1 vend-whse-trans.r-no
&Global-define DATATYP1 INTEGER
&Global-define FORMAT-1 >>>>>>>9
&Global-define SORTBY-1 BY {&FLDNAME1}
&Global-define IDXNAME1 
&Global-define DESCRIP1 Seq#
&Global-define FLDNAME2 vend-whse-trans.trans-date
&Global-define DATATYP2 DATE
&Global-define FORMAT-2 99/99/99
&Global-define SORTBY-2 BY {&FLDNAME2}
&Global-define IDXNAME2 
&Global-define DESCRIP2 Usage/Receipt Date
&Global-define FLDNAME3 vend-whse-trans.item-po-no
&Global-define FORMAT-3 x(9)
&Global-define SORTBY-3 BY {&FLDNAME3}
&Global-define IDXNAME3 
&Global-define DESCRIP3 Cust PO#
&Global-define FLDNAME4 vend-whse-trans.cust-part-no
&Global-define FORMAT-4 X(12)
&Global-define SORTBY-4 BY {&FLDNAME4}
&Global-define IDXNAME4 
&Global-define DESCRIP4 Cust Part#
&Global-define FLDNAME5 vend-whse-trans.fg-item-no
&Global-define FORMAT-5 x(15)
&Global-define SORTBY-5 BY {&FLDNAME3}
&Global-define IDXNAME5 
&Global-define DESCRIP5 FG ITEM#
/*&Global-define FLDNAME5 vend-whse-trans.i-no
&Global-define FORMAT-5 X(10)
&Global-define SORTBY-5 BY {&FLDNAME5}
&Global-define IDXNAME5 
&Global-define DESCRIP5 Item No
&Global-define FLDNAME6 vend-whse-trans.job-no
&Global-define FORMAT-6 X(6)
&Global-define SORTBY-6 BY {&FLDNAME6}
&Global-define IDXNAME6 
&Global-define DESCRIP6 Job#
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
&Global-define DESCRIP13 */
&Global-define ENHANCE yes
