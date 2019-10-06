/* quotehd.i
"ASI" "quotehd"
"cust-no" "Cust. #" ""
"q-no" "Quote #" ""
"quo-date" "Quote Date" ""
"" "" ""
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

&Global-define FLDNAME1 quotehd.q-no
&Global-define DATATYP1 INTEGER
&Global-define FORMAT-1 >>>>>
&Global-define SORTBY-1 BY quotehd.quo-date DESCENDING BY {&fldname1} DESCENDING BY quoteqty.qty
&Global-define IDXNAME1 
&Global-define DESCRIP1 Quote #
&Global-define FLDNAME2 quotehd.cust-no
&Global-define FORMAT-2 X(8)
&Global-define SORTBY-2 BY quotehd.quo-date DESCENDING BY {&fldname1} DESCENDING BY quoteqty.qty
&Global-define IDXNAME2 
&Global-define DESCRIP2 Cust #
&Global-define FLDNAME3 quotehd.quo-date
&Global-define DATATYP3 DATE
&Global-define FORMAT-3 99/99/99
&Global-define SORTBY-3 BY quotehd.quo-date DESCENDING BY {&fldname1} DESCENDING BY quoteqty.qty
&Global-define IDXNAME3 
&Global-define DESCRIP3 Quote Date
&Global-define FLDNAME4 quotehd.est-no
&Global-define FORMAT-4 X(5)
&Global-define SORTBY-4 BY quotehd.quo-date DESCENDING BY {&fldname1} DESCENDING BY quoteqty.qty
&Global-define IDXNAME4 
&Global-define DESCRIP4 Est #
&Global-define FLDNAME5 quotehd.rfq
&Global-define FORMAT-5 X(8)
&Global-define SORTBY-5 BY quotehd.quo-date DESCENDING BY {&fldname1} DESCENDING BY quoteqty.qty
&Global-define IDXNAME5 
&Global-define DESCRIP5 RFQ
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
