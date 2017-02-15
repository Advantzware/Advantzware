/* oe/rep/invxprnt.i */
DEFINE VARIABLE cEmail AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPhone AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFax   AS CHARACTER NO-UNDO.

 FIND FIRST cust NO-LOCK
     WHERE cust.company EQ cocode
       AND cust.cust-no EQ tt-word-print.cust-no NO-ERROR .
 IF AVAIL cust THEN
     ASSIGN
       cEmail = cust.email .
       cPhone = cust.phone .
       cFax   = cust.fax-prefix + "-" + cust.fax-country    .

PUT  "<FArial>".
PUT  "<C+25><#1>".
PUT  "<=1>" SKIP.
PUT  "<C1><#2>".
PUT   "<#=100><AT=,2><FROM><AT=+.8,+4><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE= " tt-word-print.tag-no FORMAT "x(20)" ">"
    "<AT=,3>" tt-word-print.tag-no FORMAT "x(20)" .

PUT  SKIP(3) "<R9><C3><FROM><R9><C80><LINE>" SKIP .

PUT "<R11><C3><#4><FROM><R11><C80><RECT><||3>" SKIP
    "<R18><C3><FROM><R18><C80><LINE><||3>" SKIP  
    "<R11><C3><FROM><R18><C3><LINE><||3>" SKIP
    "<R11><C80><FROM><R18><C80><LINE><||3>" SKIP.

PUT "<FArial><=4><R+1><C5><B>" tt-word-print.ship-name FORMAT "x(30)"  "</B>" SKIP.
PUT "<FArial><=4><R+1><C50><B>" cEmail FORMAT "x(30)"  "</B>" SKIP(2).
PUT "<FArial><=4><R+3><C5><B>" tt-word-print.ship-add1 FORMAT "x(30)" "</B>" .
PUT "<FArial><=4><R+3><C50><B>" cPhone FORMAT "x(30)" "</B>" SKIP(2).
PUT "<FArial><=4><R+5><C5><B>" tt-word-print.ship-city FORMAT "x(15)" tt-word-print.ship-state FORMAT "x(2)" tt-word-print.ship-zip  "</B>" .
PUT "<FArial><=4><R+5><C50><B>" cFax FORMAT "x(30)" "</B>" SKIP(2).

PUT "<R22><C3><#5><FROM><R22><C80><RECT><||3>" SKIP
    "<R25><C3><FROM><R25><C80><LINE><||3>" SKIP  
    "<R22><C3><FROM><R25><C3><LINE><||3>" SKIP
    "<R22><C80><FROM><R25><C80><LINE><||3>" SKIP.

PUT "<FCourier New><=5><R-2><I> Customer Name </I>" SKIP.
PUT "<FArial><=5><R+1><B>  "  tt-word-print.cust-name FORMAT "x(30)"  "</B>" SKIP.

PUT "<R28><C3><#6><FROM><R28><C40><RECT><||3>" SKIP
    "<R31><C3><FROM><R31><C40><LINE><||3>" SKIP  
    "<R28><C3><FROM><R31><C3><LINE><||3>" SKIP
    "<R28><C40><FROM><R31><C40><LINE><||3>" SKIP.

PUT "<FCourier New><=6><R-2><I> Customer PO# </I>" SKIP.
PUT "<FArial><=6><R+1><B>  " tt-word-print.cust-po-no FORMAT "x(15)"  "</B>" SKIP.

PUT "<R28><C45><#7><FROM><R28><C80><RECT><||3>" SKIP
    "<R31><C45><FROM><R31><C80><LINE><||3>" SKIP  
    "<R28><C45><FROM><R31><C45><LINE><||3>" SKIP
    "<R28><C80><FROM><R31><C80><LINE><||3>" SKIP.

PUT "<FCourier New><=7><R-2><I> Customer Job# </I>" SKIP.
PUT "<FArial><=7><R+1><B>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99") "</B>" SKIP.

PUT "<R34><C3><#8><FROM><R34><C80><RECT><||3>" SKIP
    "<R37><C3><FROM><R37><C80><LINE><||3>" SKIP  
    "<R34><C3><FROM><R37><C3><LINE><||3>" SKIP
    "<R34><C80><FROM><R37><C80><LINE><||3>" SKIP.

PUT "<FCourier New><=8><R-2><I> Item Name </I>" SKIP.
PUT "<FArial><=8><R+1><B>  " tt-word-print.i-name FORMAT "x(30)" "</B>" SKIP.

PUT "<R40><C3><#9><FROM><R40><C40><RECT><||3>" 
    "<R43><C3><FROM><R43><C40><LINE><||3>"  
    "<R40><C3><FROM><R43><C3><LINE><||3>" 
    "<R40><C40><FROM><R43><C40><LINE><||3>" .

PUT "<FCourier New><=9><R-2><I> Customer Part#</I>" SKIP.
PUT "<FArial><=9><R+1><B>  " tt-word-print.cust-part-no FORMAT "x(30)"    "</B>" SKIP.

PUT "<R40><C45><#10><FROM><R40><C80><RECT><||3>" 
    "<R43><C45><FROM><R43><C80><LINE><||3>"   
    "<R40><C45><FROM><R43><C45><LINE><||3>" 
    "<R40><C80><FROM><R43><C80><LINE><||3>" .

PUT "<FCourier New><=10><R-2><I> Finished Goods Item# </I>" SKIP.
PUT "<FArial><=10><R+1><B>  " tt-word-print.i-no FORMAT "x(15)" "</B>" SKIP.

PUT "<R46><C3><#11><FROM><R46><C18><RECT><||3>"
    "<R50><C3><FROM><R50><C18><LINE><||3>"  
    "<R46><C3><FROM><R50><C3><LINE><||3>" 
    "<R46><C18><FROM><R50><C18><LINE><||3>" .

PUT "<FArial><=11><R-2><B> Qty / Case</B>" SKIP.
PUT "<FArial><=11><R+1><B>  " tt-word-print.cas-no   "</B>" SKIP.

PUT "<R46><C22><#12><FROM><R46><C37><RECT><||3>" 
    "<R50><C22><FROM><R50><C37><LINE><||3>"  
    "<R46><C22><FROM><R50><C22><LINE><||3>" 
    "<R46><C37><FROM><R50><C37><LINE><||3>" .

PUT "<FArial><=12><R-2><B> Cases / Pallet</B>" SKIP.
PUT "<FArial><=12><R+1><B>  "  tt-word-print.bundle "</B>" SKIP.

PUT "<R46><C41><#13><FROM><R46><C56><RECT><||3>" 
    "<R50><C41><FROM><R50><C56><LINE><||3>"   
    "<R46><C41><FROM><R50><C41><LINE><||3>" 
    "<R46><C56><FROM><R50><C56><LINE><||3>" .
PUT "<FArial><=13><R-2><B> Partial Case</B>" SKIP.
PUT "<FArial><=13><R+1><B>  "  tt-word-print.partial "</B>" SKIP.

PUT "<R46><C60><#14><FROM><R46><C76><RECT><||3>" 
    "<R50><C60><FROM><R50><C76><LINE><||3>"  
    "<R46><C60><FROM><R50><C60><LINE><||3>" 
    "<R46><C76><FROM><R50><C76><LINE><||3>" .

PUT "<FArial><=14><R-2><B> Qty Pallet</B>" SKIP.
PUT "<FArial><=14><R+1><B>  "  tt-word-print.pcs  "</B>" SKIP.

PUT "<R53><C3><#15><FROM><R53><C18><RECT><||3>" 
    "<R57><C3><FROM><R57><C18><LINE><||3>"  
    "<R53><C3><FROM><R57><C3><LINE><||3>" 
    "<R53><C18><FROM><R57><C18><LINE><||3>".

PUT "<FArial><=15><R-2><B> Wt / Case</B>" SKIP.
PUT "<FArial><=15><R+1><B>  " tt-word-print.unit-wt "</B>" SKIP.

PUT "<R53><C20><#16><FROM><R53><C46><RECT><||3>" 
    "<R57><C20><FROM><R57><C46><LINE><||3>"   
    "<R53><C20><FROM><R57><C20><LINE><||3>" 
    "<R53><C46><FROM><R57><C46><LINE><||3>" .

PUT "<FCourier New><=16><R+4><C25>Counted By" SKIP.

PUT "<R53><C50><#17><FROM><R53><C76><RECT><||3>" 
    "<R57><C50><FROM><R57><C76><LINE><||3>"   
    "<R53><C50><FROM><R57><C50><LINE><||3>" 
    "<R53><C76><FROM><R57><C76><LINE><||3>" .

PUT "<FCourier New><=17><R+4><C55>Checked By" SKIP.
.

PUT "<FCourier New>".
