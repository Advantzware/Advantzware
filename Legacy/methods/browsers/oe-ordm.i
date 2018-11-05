/* oe-ordm.i
"ASI" "oe-ordm"
"actnum" "Account No" ""
"amt" "Amount" ""
"charge" "Charge" ""
"bill" "Bill" ""
"cost" "Cost" ""
"dscr" "Description" ""
"est-no" "Estimate #" ""
"line" "Line" ""
"ord-i-no" "Order Line Item" ""
"ord-line" "Order Line" ""
"ord-no" "Order#" ""
"tax" "Tax" ""
"" "" ""
"yes" "no" "no"
"no"
*/

&Global-define FLDNAME1 oe-ordm.actnum
&Global-define FORMAT-1 X(25)
&Global-define SORTBY-1 
&Global-define IDXNAME1 
&Global-define DESCRIP1 Account No
&Global-define FLDNAME2 oe-ordm.amt
&Global-define DATATYP2 DECIMAL
&Global-define FORMAT-2 ->>>,>>9.99
&Global-define SORTBY-2 
&Global-define IDXNAME2 
&Global-define DESCRIP2 Amount
&Global-define FLDNAME3 oe-ordm.charge
&Global-define FORMAT-3 X(20)
&Global-define SORTBY-3 
&Global-define IDXNAME3 
&Global-define DESCRIP3 Charge
&Global-define FLDNAME4 oe-ordm.bill
&Global-define FORMAT-4 X
&Global-define SORTBY-4 
&Global-define IDXNAME4 
&Global-define DESCRIP4 Bill
&Global-define FLDNAME5 oe-ordm.cost
&Global-define DATATYP5 DECIMAL
&Global-define FORMAT-5 ->>,>>>,>>9.99
&Global-define SORTBY-5 
&Global-define IDXNAME5 
&Global-define DESCRIP5 Cost
&Global-define FLDNAME6 oe-ordm.dscr
&Global-define FORMAT-6 X(20)
&Global-define SORTBY-6 
&Global-define IDXNAME6 
&Global-define DESCRIP6 Description
&Global-define FLDNAME7 oe-ordm.est-no
&Global-define FORMAT-7 X(5)
&Global-define SORTBY-7 
&Global-define IDXNAME7 
&Global-define DESCRIP7 Estimate #
&Global-define FLDNAME8 oe-ordm.line
&Global-define DATATYP8 INTEGER
&Global-define FORMAT-8 >9
&Global-define SORTBY-8 
&Global-define IDXNAME8 
&Global-define DESCRIP8 Line
&Global-define FLDNAME9 oe-ordm.ord-i-no
&Global-define FORMAT-9 X(15)
&Global-define SORTBY-9 
&Global-define IDXNAME9 
&Global-define DESCRIP9 Order Line Item
&Global-define FLDNAME10 oe-ordm.ord-line
&Global-define DATATYP10 INTEGER
&Global-define FORMAT-10 >9
&Global-define SORTBY-10 
&Global-define IDXNAME10 
&Global-define DESCRIP10 Order Line
&Global-define FLDNAME11 oe-ordm.ord-no
&Global-define DATATYP11 INTEGER
&Global-define FORMAT-11 >>>>>9
&Global-define SORTBY-11 
&Global-define IDXNAME11 
&Global-define DESCRIP11 Order#
&Global-define FLDNAME12 oe-ordm.tax
&Global-define DATATYP12 LOGICAL
&Global-define FORMAT-12 Y/N
&Global-define SORTBY-12 
&Global-define IDXNAME12 
&Global-define DESCRIP12 Tax
&Global-define FLDNAME13 
&Global-define SORTBY-13 
&Global-define IDXNAME13 
&Global-define DESCRIP13 
&Global-define ENHANCE yes
