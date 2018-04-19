/* oe/rep/invxprnt.i */     

IF tb_print-view THEN do:                 

        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        
         PUT "<R2><C3><P14><FROM><R2><C80><LINE>" SKIP.
         PUT "<FArial><R3><C30><p35><B> XPEDX </B>" SKIP.
         PUT "<R6><C31><P35><FROM><R6><C48><LINE>" SKIP.
         
         PUT "<||><R7><C23><#4><FROM><R11><C80><RECT>" SKIP.            
        
         PUT "<FArial><=4><R8><C3><B><P22> Customer: " "<C25>" tt-word-print.cust-name FORMAT "x(25)"  "</B>" SKIP.
        
         PUT "<||><R13><C23><#5><FROM><R17><C80><RECT>" SKIP.           
        
         PUT "<FArial><=5><R14><C3><B><P22> MAX# " "<C25>" tt-word-print.cust-name FORMAT "x(25)"   "</B>" SKIP.
        
         PUT "<||><R19><C23><#6><FROM><R23><C80><RECT>" SKIP.            
        
         PUT "<FArial><=6><R20><C3><B><P22> Size: " "<C25>" tt-word-print.cust-name FORMAT "x(25)"   "</B>" SKIP.
        
         PUT "<||><R25><C23><#7><FROM><R29><C80><RECT>" SKIP.            
        
         PUT "<FArial><=7><R26><C3><B><P22> Part # " "<C25>" tt-word-print.cust-part FORMAT "x(15)"    "</B>" SKIP.
        
         PUT "<||><R31><C23><#8><FROM><R35><C80><RECT>" SKIP.            
        
         PUT "<FArial><=8><R32><C3><B><P22> Cust PO# " "<C25>" tt-word-print.cust-po-no  FORMAT "x(15)"   "</B>" SKIP.
        
         PUT "<||><R37><C23><#9><FROM><R41><C80><RECT>" SKIP.            
        
         PUT "<FArial><=9><R38><C3><B><P22> XPEDX PO: " "<C25>" tt-word-print.po-no FORMAT ">>>>>>>>>>"    "</B>" SKIP.
        
         PUT "<||><R43><C23><#10><FROM><R47><C80><RECT>" SKIP.           
        
         PUT "<FArial><=10><R44><C3><B><P22> FG Item# " "<C25>"  tt-word-print.i-no FORMAT "x(15)"   "</B>" SKIP.
        
         PUT "<||><R49><C23><#11><FROM><R53><C80><RECT>" SKIP.            
        
         PUT "<FArial><=11><R50><C3><B><P22> DATE: " "<C25>"  tt-word-print.due-date FORMAT "99/99/99"  "</B>" SKIP.
        
         PUT "<||><R55><C23><#12><FROM><R59><C80><RECT>" SKIP.            
        
         PUT "<FArial><=12><R56><C3><B><P22> ORDER # " "<C25>" tt-word-print.ord-no FORMAT ">>>>>>>>>"   "</B>" SKIP.
        
         PUT "<||><R61><C23><#13><FROM><R65><C80><RECT>" SKIP.            
        
         PUT "<FArial><=13><R62><C3><B><P22> UNIT QTY: " "<C25>" tt-word-print.total-unit FORMAT ">,>>>,>>9"    "</B>" SKIP.
        
         .
        
        PUT "<FCourier New>".
END.
ELSE DO:

        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        
         PUT "<R17><C3><P14><FROM><R17><C80><LINE>" SKIP.
         PUT "<FArial><R18><C30><p35><B> XPEDX </B>" SKIP.
         PUT "<R21><C31><P35><FROM><R21><C48><LINE>" SKIP.
         
         PUT "<||><R22><C23><#4><FROM><R26><C80><RECT>" SKIP.            
        
         PUT "<FArial><=4><R23><C3><B><P22> Customer: " "<C25>" tt-word-print.cust-name FORMAT "x(25)"  "</B>" SKIP.
        
         PUT "<||><R28><C23><#5><FROM><R32><C80><RECT>" SKIP.            
        
         PUT "<FArial><=5><R29><C3><B><P22> MAX# " "<C25>" tt-word-print.cust-name FORMAT "x(25)"   "</B>" SKIP.
        
         PUT "<||><R34><C23><#6><FROM><R38><C80><RECT>" SKIP.            
        
         PUT "<FArial><=6><R35><C3><B><P22> Size: " "<C25>" tt-word-print.cust-name FORMAT "x(25)"   "</B>" SKIP.
        
         PUT "<||><R40><C23><#7><FROM><R44><C80><RECT>" SKIP.            
        
         PUT "<FArial><=7><R41><C3><B><P22> Part # " "<C25>" tt-word-print.cust-part FORMAT "x(15)"    "</B>" SKIP.
        
         PUT "<||><R46><C23><#8><FROM><R50><C80><RECT>" SKIP.           
        
         PUT "<FArial><=8><R47><C3><B><P22> Cust PO# " "<C25>" tt-word-print.cust-po-no  FORMAT "x(15)"   "</B>" SKIP.
        
         PUT "<||><R52><C23><#9><FROM><R56><C80><RECT>" SKIP.           
        
         PUT "<FArial><=9><R53><C3><B><P22> XPEDX PO: " "<C25>" tt-word-print.po-no FORMAT ">>>>>>>>>>"    "</B>" SKIP.
        
         PUT "<||><R58><C23><#10><FROM><R62><C80><RECT>" SKIP.           
        
         PUT "<FArial><=10><R59><C3><B><P22> FG Item# " "<C25>"  tt-word-print.i-no FORMAT "x(15)"   "</B>" SKIP.
        
         PUT "<||><R64><C23><#11><FROM><R68><C80><RECT>" SKIP.            
        
         PUT "<FArial><=11><R65><C3><B><P22> DATE: " "<C25>"  tt-word-print.due-date FORMAT "99/99/99"  "</B>" SKIP.
        
         PUT "<||><R70><C23><#12><FROM><R74><C80><RECT>" SKIP.            
        
         PUT "<FArial><=12><R71><C3><B><P22> ORDER # " "<C25>" tt-word-print.ord-no FORMAT ">>>>>>>>>"   "</B>" SKIP.
        
         PUT "<||><R76><C23><#13><FROM><R80><C80><RECT>" SKIP.            
        
         PUT "<FArial><=13><R77><C3><B><P22> UNIT QTY: " "<C25>" tt-word-print.total-unit FORMAT ">,>>>,>>9"    "</B>" SKIP.
        
         .
        
        PUT "<FCourier New>".

END.








