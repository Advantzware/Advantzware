/* oe/rep/invxprnt.i */ 


DEFINE VARIABLE cSize AS CHARACTER.

cSize = string(itemfg.l-score[50]) + "X" + STRING(itemfg.w-score[50]) + "X" + string(itemfg.d-score[50]) .
  

IF iplPrintView THEN do:                 

        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        PUT UNFORMATTED   
               "<C4><R15.5><#1><R+10><C+70><IMAGE#1=" ls-full-img1.
        
         PUT "<R33><C30><P22><FROM><R33><C80><LINE>" SKIP.
        
         PUT "<FArial><=2><R30> <B><P18> CUSTOMER NAME: "  "<C30>" tt-word-print.cust-name FORMAT "x(30)"   "</B>" SKIP.
        
         PUT "<R38><C15><P22><FROM><R38><C80><LINE>" SKIP.
        
         PUT "<FArial><=3><R35> <B><P21> PART#: "  "<C16>"  tt-word-print.cust-part-no FORMAT "x(15)"   "</B>" SKIP.
        
         PUT "<R43><C27><P22><FROM><R43><C80><LINE>" SKIP.
        
         PUT "<FArial><=4><R40> <B><P21> PO# / ORDER#: "  "<C27>"  tt-word-print.po-no FORMAT ">>>>>>>>>"  "<C40>" "/"  tt-word-print.ord-no FORMAT ">>>>>>>"    "</B>" SKIP.
        
         PUT "<R48><C11><P22><FROM><R48><C80><LINE>" SKIP.
        
         PUT "<FArial><=5><R45><B><P21> SIZE: "  "<C11>"  cSize "</B>".
        
         PUT "<R53><C14><P22><FROM><R53><C80><LINE>" SKIP.
        
         PUT "<FArial><=6><R50><B><P21> DATE: "  "<C14>"  tt-word-print.due-date FORMAT "99/99/99"   "</B>".
        
         PUT "<R59><C20><P22><FROM><R59><C80><LINE>".
        
         PUT "<FArial><=7><R56><B><P21> QUANTITY: "   "<C20>"  tt-word-print.ord-qty FORMAT "->,>>>,>>9"    "</B>".
        
         PUT "<FArial><=9><R62.5><C7> <B><P22> "    tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99")  "</B>" SKIP.
         PUT "<FArial><=9><R62.6><C37> <B><P22> "    tt-word-print.i-no FORMAT "x(15)"  "</B>" SKIP.
         .
        
        PUT "<FCourier New>".
END.

ELSE DO:

        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        PUT UNFORMATTED   
               "<C4><R25.5><#1><R+10><C+70><IMAGE#1=" ls-full-img1.
        
         PUT "<R43><C30><P22><FROM><R43><C80><LINE>" SKIP.
        
         PUT "<FArial><=2><R40> <B><P18> CUSTOMER NAME: "  "<C30>" tt-word-print.cust-name FORMAT "x(30)"   "</B>" SKIP.
        
         PUT "<R48><C15><P22><FROM><R48><C80><LINE>" SKIP.
        
         PUT "<FArial><=3><R45> <B><P21> PART#: "  "<C16>"  tt-word-print.cust-part-no FORMAT "x(15)"   "</B>" SKIP.
        
         PUT "<R53><C27><P22><FROM><R53><C80><LINE>" SKIP.
        
         PUT "<FArial><=4><R50> <B><P21> PO# / ORDER#: "  "<C27>"  tt-word-print.po-no FORMAT ">>>>>>>>>"  "<C40>" "/"  tt-word-print.ord-no FORMAT ">>>>>>>"    "</B>" SKIP.
        
         PUT "<R58><C11><P22><FROM><R58><C80><LINE>" SKIP.
        
         PUT "<FArial><=5><R55><B><P21> SIZE: "  "<C11>"  cSize  "</B>".
        
         PUT "<R63><C14><P22><FROM><R63><C80><LINE>" SKIP.
        
         PUT "<FArial><=6><R60><B><P21> DATE: "  "<C14>"  tt-word-print.due-date FORMAT "99/99/99"   "</B>".
        
         PUT "<R69><C20><P22><FROM><R69><C80><LINE>".
        
         PUT "<FArial><=7><R66><B><P21> QUANTITY: "   "<C20>"  tt-word-print.ord-qty FORMAT "->,>>>,>>9"    "</B>".
        
         PUT "<FArial><=9><R72.5><C7> <B><P22> "    tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99")  "</B>" SKIP.
         PUT "<FArial><=9><R72.5><C37> <B><P22> "    tt-word-print.i-no FORMAT "x(15)"  "</B>" SKIP.
         .
        
        PUT "<FCourier New>".

END. /* else do */







