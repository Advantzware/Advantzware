/* oe/rep/invxprnt.i */  

FIND FIRST cust NO-LOCK
     WHERE cust.company EQ cocode
       AND cust.active = "X" NO-ERROR .
 IF AVAIL cust THEN
     ASSIGN
       cEmail = cust.email .
       cPhone = string(cust.area-code,"(xxx)") +  string(cust.phone,"xxx-xxxx") .
       cFax   = STRING(SUBSTR(cust.fax,1,3),"(xxx)") + "-" + STRING(SUBSTR(cust.fax,4),"xxx-xxxx")   .
     FIND FIRST company NO-LOCK
          WHERE company.company  = cocode  NO-ERROR.          
     IF tb_print-view THEN do: 
        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        PUT UNFORMATTED   
               "<C4><R3><#1><R+9><C+70><IMAGE#1=" ls-full-img1.
        
         PUT "<R17><C3><P25><FROM><R17><C80><LINE>" SKIP.
        
         PUT "<FArial><R15><C3><p12><B>" company.addr[1] FORMAT "x(30)"  "</B>" .
         PUT "<FArial><=4><R15><C27><p11><B>" STRING(company.city,"x(15)") " " company.state FORMAT "x(2)" " " company.zip "</B>" .
        
         PUT "<FArial><=4><R15><C44><p12><B>        Phone: " cPhone FORMAT "x(15)"  "</B>" .
         PUT "<FArial><=4><R15><C64><p12><B>   Fax: " cFax FORMAT "x(15)"  "</B>" SKIP.
        
         PUT "<||><R20><C3><#4><FROM><R25><C80><RECT>" SKIP.            
        
        PUT "<FArial><=4><R18><C3><B><p15> Customer Name </B>" SKIP.
        PUT "<FArial><=4><R21.2><C3><B><P24>  " tt-word-print.cust-name FORMAT "x(27)" "</B>" SKIP.
        
        PUT "<||><R28><C3><#5><FROM><R33><C40><RECT>" SKIP.            
        
        PUT "<FArial><=5><R-2><B><p15> Finished Goods Item#                                  Item Name </B>" SKIP.
        PUT "<FArial><=5><R29.5><C3><B><P18>  " tt-word-print.i-no FORMAT "x(15)" "</B>" SKIP.
        
        
        PUT "<||><R28><C45><#6><FROM><R33><C80><RECT>" SKIP.            
        
        PUT "<FArial><=6><R29.5><C46><B><P17>  " tt-word-print.i-name FORMAT "x(25)"   "</B>" SKIP.
        
        PUT "<||><R36><C3><#7><FROM><R41><C40><RECT>" SKIP.            
        
        PUT "<FArial><=7><R-2><B><p15> Job#                                                               Date </B>" SKIP.
        PUT "<FArial><=7><R37.3><C3><B><P20>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99")  "</B>" SKIP.
        
        
        PUT "<||><R36><C45><#8><FROM><R41><C80><RECT>" SKIP.           
        
        PUT "<FArial><=8><R37.3><C45><B><P20>  " tt-word-print.due-date FORMAT "99/99/99" "</P20></B>" SKIP.
        
        PUT   "<#=100><AT=7.6,0.6><FROM><AT=+.8,+4><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.cust-part-no FORMAT "x(15)" ">"
            "<AT=7.3,2.08><p15>" tt-word-print.cust-part-no FORMAT "x(15)" .
        
        PUT "<||><R44><C3><#9><FROM><R54><C50><RECT>" SKIP.            
        
        PUT "<FArial><=4><R42><C3><B><p15> Customer Part# </B>" SKIP.
        
        PUT   "<#=100><AT=7.6,5.8><FROM><AT=+.8,+2><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.ord-qty FORMAT "->,>>>,>>9" ">"
            "<AT=7.3,6.1>" tt-word-print.ord-qty FORMAT "->,>>>,>>9" .
        
        PUT "<||><R44><C53><#9><FROM><R54><C80><RECT>" SKIP.            
        PUT "<FArial><=4><R42><C65><B><p15> Qty </B>" SKIP.
        
        PUT   "<#=100><AT=9.75,3.4><FROM><AT=+.8,+2><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.cust-po-no FORMAT "x(15)" ">"
            "<AT=9.45,3.57>" tt-word-print.cust-po-no FORMAT "x(15)"  .
        
        PUT "<||><R57><C25><#9><FROM><R65><C65><RECT>" SKIP.            
        PUT "<FArial><=4><R55><C36><B><p15> Customer Po# </B>" SKIP.
        
         .
        
        PUT "<FCourier New>".
     END.  

     ELSE do: 
        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        PUT UNFORMATTED   
               "<C4><R15><#1><R+9><C+70><IMAGE#1=" ls-full-img1.
        
         PUT "<R32><C3><P25><FROM><R32><C80><LINE>" SKIP.
        
         PUT "<FArial><R30><C3><p12><B>" company.addr[1] FORMAT "x(30)"  "</B>" .
         PUT "<FArial><=4><R30><C27><p11><B>" STRING(company.city,"x(15)") " " company.state FORMAT "x(2)" " " company.zip "</B>" .
        
         PUT "<FArial><=4><R30><C44><p12><B>        Phone: " cPhone FORMAT "x(15)"  "</B>" .
         PUT "<FArial><=4><R30><C64><p12><B>   Fax: " cFax FORMAT "x(15)"  "</B>" SKIP.
        
         PUT "<||><R35><C3><#4><FROM><R40><C80><RECT>" SKIP.            
        
        PUT "<FArial><=4><R33><C3><B><p15> Customer Name </B>" SKIP.
        PUT "<FArial><=4><R36.2><C3><B><P24>  " tt-word-print.cust-name FORMAT "x(27)" "</B>" SKIP.
        
        PUT "<||><R43><C3><#5><FROM><R48><C40><RECT>" SKIP.            
        
        PUT "<FArial><=5><R-2><B><p15> Finished Goods Item#                                  Item Name </B>" SKIP.
        PUT "<FArial><=5><R44.5><C3><B><P18>  " tt-word-print.i-no FORMAT "x(15)" "</B>" SKIP.
        
        
        PUT "<||><R43><C45><#6><FROM><R48><C80><RECT>" SKIP.            
        
        PUT "<FArial><=6><R44.5><C46><B><P17>  " tt-word-print.i-name FORMAT "x(25)"   "</B>" SKIP.
        
        PUT "<||><R51><C3><#7><FROM><R56><C40><RECT>" SKIP.            
        
        PUT "<FArial><=7><R-2><B><p15> Job#                                                               Date </B>" SKIP.
        PUT "<FArial><=7><R52.3><C3><B><P20>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99")  "</B>" SKIP.
        
        
        PUT "<||><R51><C45><#8><FROM><R56><C80><RECT>" SKIP.            
        
        PUT "<FArial><=8><R52.3><C45><B><P20>  " tt-word-print.due-date FORMAT "99/99/99" "</P20></B>" SKIP.
        
        PUT   "<#=100><AT=10.00,0.6><FROM><AT=+.8,+4><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.cust-part-no FORMAT "x(15)" ">"
            "<AT=10.90,2.08><p15>" tt-word-print.cust-part-no FORMAT "x(15)" .
        
        PUT "<||><R59><C3><#9><FROM><R69><C50><RECT>" SKIP.            
        
        PUT "<FArial><=4><R57><C3><B><p15> Customer Part# </B>" SKIP.
        
        PUT   "<#=100><AT=10.00,5.8><FROM><AT=+.8,+2><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.ord-qty FORMAT "->,>>>,>>9" ">"
            "<AT=10.90,6.1>" tt-word-print.ord-qty FORMAT "->,>>>,>>9"  .
        
        PUT "<||><R59><C53><#9><FROM><R69><C80><RECT>" SKIP.
            
        PUT "<FArial><=4><R57><C65><B><p15> Qty </B>" SKIP.
        
        PUT   "<#=100><AT=12.00,3.4><FROM><AT=+.8,+2><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.cust-po-no FORMAT "x(15)" ">"
            "<AT=12.90,3.57>" tt-word-print.cust-po-no FORMAT "x(15)"  .
        
        PUT "<||><R72><C25><#9><FROM><R80><C65><RECT>" SKIP.
            
        PUT "<FArial><=4><R70><C36><B><p15> Customer Po# </B>" SKIP.

                
         .
        
        PUT "<FCourier New>".
     END. /* else do*/







