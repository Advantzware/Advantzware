/* oe/rep/invxprnt.i */  

IF tb_print-view THEN do:                   

        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        PUT UNFORMATTED   
               "<C4><R3><#1><R+9><C+70><IMAGE#1=" ls-full-img1.
        
         PUT "<R16><C3><P25><FROM><R16><C80><LINE>" SKIP.
        
         PUT "<||><R19><C3><#4><FROM><R25><C80><RECT>" SKIP.            
        
        PUT "<FArial><=4><R17><C3><B><p15> Customer Name </B>" SKIP.
        PUT "<FArial><=4><R20><C3><B><P23>  " tt-word-print.cust-name FORMAT "x(30)"   "</B>" SKIP.
        PUT "<FArial><=4><R23><C3><B><P23>  PO# " tt-word-print.cust-po-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<||><R28><C3><#5><FROM><R33><C40><RECT>" SKIP.            
        
        PUT "<FArial><=5><R-2><B><p15> FG#                                                                Cust Part# </B>" SKIP.
        PUT "<FArial><=5><R29.5><C3><B><P18>  " tt-word-print.i-no FORMAT "x(15)"   "</B>" SKIP.
        
        
        PUT "<||><R28><C45><#6><FROM><R33><C80><RECT>" SKIP.            
        
        PUT "<FArial><=6><R29.5><C46><B><P18>  " tt-word-print.cust-part-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<||><R36><C3><#7><FROM><R41><C40><RECT>" SKIP.            
        
        PUT "<FArial><=7><R-2><B><p15> Job#                                                               Date </B>" SKIP.
        PUT "<FArial><=7><R37.5><C3><B><P20>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99")  "</B>" SKIP.
        
        
        PUT "<||><R36><C45><#8><FROM><R41><C80><RECT>" SKIP.            
        
        PUT "<FArial><=8><R37.5><C45><B><P20>  " tt-word-print.due-date FORMAT "99/99/99"    "</B>" SKIP.
        
        PUT   "<#=100><AT=7.6,0.6><FROM><AT=+.8,+3.1><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.i-no FORMAT "x(30)"   ">"
            "<AT=7.3,1><p15>" tt-word-print.i-no FORMAT "x(15)"   .
        
        PUT "<||><R44><C3><#9><FROM><R54><C40><RECT>" SKIP.            
        
        PUT "<FArial><=4><R42><C3><B><p15> Finished Goods Item# (VPN) </B>" SKIP.
        
        PUT   "<#=100><AT=7.6,5.4><FROM><AT=+.8,+2><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.ord-qty FORMAT "->,>>>,>>9"   ">"
            "<AT=7.3,5.7>" tt-word-print.ord-qty FORMAT "->,>>>,>>9"   .
        
        PUT "<||><R44><C45><#9><FROM><R54><C80><RECT>" SKIP.
            
        PUT "<FArial><=4><R42><C45><B><p15> Quantity</B>" SKIP.
         .
        
        PUT "<FCourier New>".
END.
ELSE DO:

        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        PUT UNFORMATTED   
               "<C4><R23><#1><R+9><C+70><IMAGE#1=" ls-full-img1.
        
         PUT "<R36><C3><P25><FROM><R36><C80><LINE>" SKIP.
        
         PUT "<||><R39><C3><#4><FROM><R45><C80><RECT>" SKIP.            
        
        PUT "<FArial><=4><R37><C3><B><p15> Customer Name </B>" SKIP.
        PUT "<FArial><=4><R40><C3><B><P23>  " tt-word-print.cust-name FORMAT "x(30)"   "</B>" SKIP.
        PUT "<FArial><=4><R43><C3><B><P23>  PO# " tt-word-print.cust-po-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<||><R48><C3><#5><FROM><R53><C40><RECT>" SKIP.            
        
        PUT "<FArial><=5><R-2><B><p15> FG#                                                                Cust Part# </B>" SKIP.
        PUT "<FArial><=5><R49.5><C3><B><P18>  " tt-word-print.i-no FORMAT "x(15)"   "</B>" SKIP.
        
        
        PUT "<||><R48><C45><#6><FROM><R53><C80><RECT>" SKIP.
            
        
        PUT "<FArial><=6><R49.5><C46><B><P18>  " tt-word-print.cust-part-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<||><R56><C3><#7><FROM><R61><C40><RECT>" SKIP.            
        
        PUT "<FArial><=7><R-2><B><p15> Job#                                                               Date </B>" SKIP.
        PUT "<FArial><=7><R57.5><C3><B><P20>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99")  "</B>" SKIP.
        
        
        PUT "<||><R56><C45><#8><FROM><R61><C80><RECT>" SKIP.            
        
        PUT "<FArial><=8><R57.5><C45><B><P20>  " tt-word-print.due-date FORMAT "99/99/99"    "</B>" SKIP.
        
        PUT   "<#=100><AT=10.85,0.6><FROM><AT=+.8,+3.1><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.i-no FORMAT "x(30)"   ">"
            "<AT=11.70,1><p15>" tt-word-print.i-no FORMAT "x(15)"   .
        
        PUT "<||><R64><C3><#9><FROM><R74><C40><RECT>" SKIP.            
        
        PUT "<FArial><=4><R62><C3><B><p15> Finished Goods Item# (VPN) </B>" SKIP.
        
        PUT   "<#=100><AT=10.85,5.4><FROM><AT=+.8,+2><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.ord-qty FORMAT "->,>>>,>>9"   ">"
            "<AT=11.70,5.7>" tt-word-print.ord-qty FORMAT "->,>>>,>>9"   .
        
        PUT "<||><R64><C45><#9><FROM><R74><C80><RECT>" SKIP.
            
        PUT "<FArial><=4><R62><C45><B><p15> Quantity</B>" SKIP.
         .
        
        PUT "<FCourier New>".

END. /* else do */







