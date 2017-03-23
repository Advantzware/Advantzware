/* oe/rep/invxprnt.i */                     

PUT  "<FArial>".
PUT  "<C+25><#1>".
PUT  "<=1>" SKIP.
PUT  "<C1><#2>".

 PUT "<R2><C3><P14><FROM><R2><C80><LINE>" SKIP.
 PUT "<FArial><R3><C30><p35><B> XPEDX </B>" SKIP.
 PUT "<R6><C31><P35><FROM><R6><C48><LINE>" SKIP.
 
 PUT "<R7><C23><#4><FROM><R7><C80><RECT><||3>" SKIP
    "<R11><C23><FROM><R11><C80><LINE><||3>" SKIP  
    "<R7><C23><FROM><R11><C23><LINE><||3>" SKIP
    "<R7><C80><FROM><R11><C80><LINE><||3>" SKIP.

 PUT "<FArial><=4><R8><C3><B><P22> Customer: " "<C25>" tt-word-print.cust-name FORMAT "x(25)"  "</B>" SKIP.

 PUT "<R13><C23><#5><FROM><R13><C80><RECT><||3>" SKIP
    "<R17><C23><FROM><R17><C80><LINE><||3>" SKIP  
    "<R13><C23><FROM><R17><C23><LINE><||3>" SKIP
    "<R13><C80><FROM><R17><C80><LINE><||3>" SKIP.

 PUT "<FArial><=5><R14><C3><B><P22> MAX# " "<C25>" tt-word-print.cust-name FORMAT "x(25)"   "</B>" SKIP.

 PUT "<R19><C23><#6><FROM><R19><C80><RECT><||3>" SKIP
    "<R23><C23><FROM><R23><C80><LINE><||3>" SKIP  
    "<R19><C23><FROM><R23><C23><LINE><||3>" SKIP
    "<R19><C80><FROM><R23><C80><LINE><||3>" SKIP.

 PUT "<FArial><=6><R20><C3><B><P22> Size: " "<C25>" tt-word-print.cust-name FORMAT "x(25)"   "</B>" SKIP.

 PUT "<R25><C23><#7><FROM><R25><C80><RECT><||3>" SKIP
    "<R29><C23><FROM><R29><C80><LINE><||3>" SKIP  
    "<R25><C23><FROM><R29><C23><LINE><||3>" SKIP
    "<R25><C80><FROM><R29><C80><LINE><||3>" SKIP.

 PUT "<FArial><=7><R26><C3><B><P22> Part # " "<C25>" tt-word-print.cust-part FORMAT "x(15)"    "</B>" SKIP.

 PUT "<R31><C23><#8><FROM><R31><C80><RECT><||3>" SKIP
    "<R35><C23><FROM><R35><C80><LINE><||3>" SKIP  
    "<R31><C23><FROM><R35><C23><LINE><||3>" SKIP
    "<R31><C80><FROM><R35><C80><LINE><||3>" SKIP.

 PUT "<FArial><=8><R32><C3><B><P22> Cust PO# " "<C25>" tt-word-print.cust-po-no  FORMAT "x(15)"   "</B>" SKIP.

 PUT "<R37><C23><#9><FROM><R37><C80><RECT><||3>" SKIP
    "<R41><C23><FROM><R41><C80><LINE><||3>" SKIP  
    "<R37><C23><FROM><R41><C23><LINE><||3>" SKIP
    "<R37><C80><FROM><R41><C80><LINE><||3>" SKIP.

 PUT "<FArial><=9><R38><C3><B><P22> XPEDX PO: " "<C25>" tt-word-print.po-no FORMAT ">>>>>>>>>>"    "</B>" SKIP.

 PUT "<R43><C23><#10><FROM><R43><C80><RECT><||3>" SKIP
    "<R47><C23><FROM><R47><C80><LINE><||3>" SKIP  
    "<R43><C23><FROM><R47><C23><LINE><||3>" SKIP
    "<R43><C80><FROM><R47><C80><LINE><||3>" SKIP.

 PUT "<FArial><=10><R44><C3><B><P22> FG Item# " "<C25>"  tt-word-print.i-no FORMAT "x(15)"   "</B>" SKIP.

 PUT "<R49><C23><#11><FROM><R49><C80><RECT><||3>" SKIP
    "<R53><C23><FROM><R53><C80><LINE><||3>" SKIP  
    "<R49><C23><FROM><R53><C23><LINE><||3>" SKIP
    "<R49><C80><FROM><R53><C80><LINE><||3>" SKIP.

 PUT "<FArial><=11><R50><C3><B><P22> DATE: " "<C25>"  tt-word-print.due-date FORMAT "99/99/99"  "</B>" SKIP.

 PUT "<R55><C23><#12><FROM><R55><C80><RECT><||3>" SKIP
    "<R59><C23><FROM><R59><C80><LINE><||3>" SKIP  
    "<R55><C23><FROM><R59><C23><LINE><||3>" SKIP
    "<R55><C80><FROM><R59><C80><LINE><||3>" SKIP.

 PUT "<FArial><=12><R56><C3><B><P22> ORDER # " "<C25>" tt-word-print.ord-no FORMAT ">>>>>>>>>"   "</B>" SKIP.

 PUT "<R61><C23><#13><FROM><R61><C80><RECT><||3>" SKIP
    "<R65><C23><FROM><R65><C80><LINE><||3>" SKIP  
    "<R61><C23><FROM><R65><C23><LINE><||3>" SKIP
    "<R61><C80><FROM><R65><C80><LINE><||3>" SKIP.

 PUT "<FArial><=13><R62><C3><B><P22> UNIT QTY: " "<C25>" tt-word-print.total-unit FORMAT ">,>>>,>>9"    "</B>" SKIP.

 .

PUT "<FCourier New>".








