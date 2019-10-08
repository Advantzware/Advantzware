/* oe/rep/invxprnt.i */
DEFINE VARIABLE cEmail AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPhone AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTagno   AS CHARACTER NO-UNDO.
ASSIGN cTagno = STRING(w-ord.i-no,"x(15)") + STRING(w-ord.job-no,"x(6)") + STRING(w-ord.job-no2,"99") .

PUT  "<FArial>".
PUT  "<C+25><#1>".
PUT  "<=1>" SKIP.
PUT  "<C1><#2>".

PUT "<FArial><R6><C20><P16><B> CUSTOMER: </B>" SKIP.
PUT "<FArial><R6><C35><P20><B>  "  w-ord.cust-no FORMAT "x(30)"  "</B>" SKIP.

PUT  SKIP(3) "<R8><C10><FROM><R8><C80><B><LINE><B>" SKIP .

PUT "<R9><C22><#4><FROM><R9><C60><RECT><||3>" SKIP
    "<R14><C22><FROM><R14><C60><LINE><||3>" SKIP  
    "<R9><C22><FROM><R14><C22><LINE><||3>" SKIP
    "<R9><C60><FROM><R14><C60><LINE><||3>" SKIP.

PUT "<FArial><R10><C10><P20><B> P / N#: </B>" SKIP.
PUT "<FArial><R10><C22><P20><B>  "  w-ord.cust-part-no FORMAT "x(15)"   "</B>" SKIP.

PUT "<FArial><R15><C10><P16><B> PO#: </B>" SKIP.
PUT "<FArial><R15><C17><P16><B>  "  w-ord.cust-po-no FORMAT "x(15)"  "</B>" SKIP.

PUT "<FArial><R15><C67><P20><B> DATE: </B>" SKIP.


PUT "<FArial><R17><C10><P20><B> IPI#: </B>" SKIP.
PUT "<FArial><R17><C17><P20><B>  "  caps(w-ord.i-no) FORMAT "x(15)"   "</B>" SKIP.
PUT "<FArial><R17><C67><B>  "  TODAY    "</B>" SKIP.

PUT "<FArial><R19><C10><P16><B> JOB#: </B>" SKIP.
PUT "<FArial><R19><C17><P16><B>  "  w-ord.job-no FORMAT "x(6)" "-" STRING(w-ord.job-no2,"99")  "</B>" SKIP.

PUT "<FArial><R23><C28><B> QTY: </B>" SKIP.
PUT "<FArial><R23><C35><B>  "  w-ord.pcs " /CS"  "</B>" SKIP(5).

PUT   "<#=100><AT=,2><FROM><AT=+.8,+4><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE= " cTagno FORMAT "x(24)"  ">"
    "<AT=,2.5>" cTagno FORMAT "x(24)"  .

