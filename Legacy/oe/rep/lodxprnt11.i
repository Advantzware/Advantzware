/* oe/rep/invxprnt.i */

 FIND FIRST cust NO-LOCK
     WHERE cust.company EQ cocode
       AND cust.cust-no EQ tt-word-print.cust-no NO-ERROR .
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
        PUT   "<#=100><AT=,2><FROM><AT=+.8,+4><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.tag-no FORMAT "x(20)"   ">"
            "<AT=,3>" tt-word-print.tag-no FORMAT "x(20)"   .
        
        PUT  SKIP(3) "<R9><C3><FROM><R9><C80><LINE>" SKIP .
        
        PUT "<R10><C3><#4><FROM><R10><C80><RECT><||3>" SKIP
            "<R17><C3><FROM><R17><C80><LINE><||3>" SKIP  
            "<R10><C3><FROM><R17><C3><LINE><||3>" SKIP
            "<R10><C80><FROM><R17><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=4><R11><C18><P20><B>"    company.NAME FORMAT "x(25)"   "</B>" SKIP.
        
        PUT "<FArial><=4><R13><C5><P16><B>"  tt-word-print.ship-add1 FORMAT "x(25)"   "</B>" .
        PUT "<FArial><=4><R13><C48><P16><B> Phone: " cPhone FORMAT "x(15)"    "</B>" SKIP(2).
        PUT "<FArial><=4><R15><C5><P16><B>" STRING(tt-word-print.ship-city,"x(15)") " " tt-word-print.ship-state FORMAT "x(2)" " "  tt-word-print.ship-zip "</B>" .
        PUT "<FArial><=4><R15><C48><P16><B> Fax: " cFax FORMAT "x(15)"  "</B>" SKIP(2).
        
        PUT "<R20><C3><#5><FROM><R20><C80><RECT><||3>" SKIP
            "<R25><C3><FROM><R25><C80><LINE><||3>" SKIP  
            "<R20><C3><FROM><R25><C3><LINE><||3>" SKIP
            "<R20><C80><FROM><R25><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=5><R-2><B><p14> Customer Name </B>" SKIP.
        PUT "<FArial><=5><R+1><B><P27>  " tt-word-print.cust-name FORMAT "x(25)"  "</B>" SKIP.
        
        PUT "<R28><C3><#6><FROM><R28><C80><RECT><||3>" SKIP
            "<R32><C3><FROM><R32><C80><LINE><||3>" SKIP  
            "<R28><C3><FROM><R32><C3><LINE><||3>" SKIP
            "<R28><C80><FROM><R32><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=6><R-2><B><p14> Customer PO# </B>" SKIP.
        PUT "<FArial><=6><R29><C4><B><P24>  " tt-word-print.cust-po-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<R35><C3><#7><FROM><R35><C80><RECT><||3>" SKIP
            "<R39><C3><FROM><R39><C80><LINE><||3>" SKIP  
            "<R35><C3><FROM><R39><C3><LINE><||3>" SKIP
            "<R35><C80><FROM><R39><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=7><R-2><B><P14> Item Name </B>" SKIP.
        PUT "<FArial><=7><R35.5><C2><B><P30>  " tt-word-print.i-name FORMAT "x(25)"  "</B>" SKIP.
        
        PUT "<R42><C3><#8><FROM><R42><C80><RECT><||3>" SKIP
            "<R48><C3><FROM><R48><C80><LINE><||3>" SKIP  
            "<R42><C3><FROM><R48><C3><LINE><||3>" SKIP
            "<R42><C80><FROM><R48><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=8><R-2><B><P14> Customer Part# </B>" SKIP.
        PUT "<FArial><=8><R43.3><C2><B><P30>  " tt-word-print.cust-part-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<R51><C3><#9><FROM><R51><C80><RECT><||3>" SKIP
            "<R55><C3><FROM><R55><C80><LINE><||3>" SKIP  
            "<R51><C3><FROM><R55><C3><LINE><||3>" SKIP
            "<R51><C80><FROM><R55><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=9><R-2><B><P14> Finished Goods Item# </B>" SKIP.
        PUT "<FArial><=9><R51.5><C2><B><P30>  " tt-word-print.i-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<R58><C3><#10><FROM><R58><C20><RECT><||3>" SKIP
            "<R63><C3><FROM><R63><C20><LINE><||3>" SKIP  
            "<R58><C3><FROM><R63><C3><LINE><||3>" SKIP
            "<R58><C20><FROM><R63><C20><LINE><||3>" SKIP.
        
        PUT "<FArial><=10><R56><C4><B><P22> Qty/Pallet </B>" SKIP.
        PUT "<FArial><=10><R59><C4><B><P24>  " tt-word-print.total-unit FORMAT ">,>>>,>>9" "</B>" SKIP.
        
        PUT "<R58><C23><#11><FROM><R58><C48><RECT><||3>" SKIP
            "<R63><C23><FROM><R63><C48><LINE><||3>" SKIP  
            "<R58><C23><FROM><R63><C23><LINE><||3>" SKIP
            "<R58><C48><FROM><R63><C48><LINE><||3>" SKIP.
        
        PUT "<FArial><=10><R56><C22><B><P22> Date</B>" SKIP.
        PUT "<FArial><=10><R59><C25><B><P24>  " tt-word-print.due-date FORMAT "99/99/9999" "</B>" SKIP.
        
        PUT "<R58><C51><#14><FROM><R58><C80><RECT><||3>" SKIP
            "<R63><C51><FROM><R63><C80><LINE><||3>" SKIP  
            "<R58><C51><FROM><R63><C51><LINE><||3>" SKIP
            "<R58><C80><FROM><R63><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=10><R56><C50><B><P22> Job# </B>" SKIP.
        PUT "<FArial><=10><R59><C55><B><P24>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99")  "</B><P12>" SKIP.
        .
        
        PUT "<FCourier New>".
END.
ELSE DO:
            
        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        PUT   "<#=100><AT=3.30,2><FROM><AT=+.8,+4><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.tag-no FORMAT "x(20)"  ">"
            "<AT=4.25,3>" tt-word-print.tag-no FORMAT "x(20)"  .
        
        PUT  SKIP(3) "<R29><C3><FROM><R29><C80><LINE>" SKIP .
        
        PUT "<R30><C3><#4><FROM><R30><C80><RECT><||3>" SKIP
            "<R37><C3><FROM><R37><C80><LINE><||3>" SKIP  
            "<R30><C3><FROM><R37><C3><LINE><||3>" SKIP
            "<R30><C80><FROM><R37><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=4><R31><C18><P20><B>"    company.NAME FORMAT "x(25)"   "</B>" SKIP.
        
        PUT "<FArial><=4><R33><C5><P16><B>"  tt-word-print.ship-add1 FORMAT "x(25)"   "</B>" .
        PUT "<FArial><=4><R33><C48><P16><B> Phone: " cPhone FORMAT "x(15)"    "</B>" SKIP(2).
        PUT "<FArial><=4><R35><C5><P16><B>" STRING(tt-word-print.ship-city,"x(15)") " " tt-word-print.ship-state FORMAT "x(2)" " "  tt-word-print.ship-zip "</B>" .
        PUT "<FArial><=4><R35><C48><P16><B> Fax: " cFax FORMAT "x(15)"  "</B>" SKIP(2).
        
        PUT "<R40><C3><#5><FROM><R40><C80><RECT><||3>" SKIP
            "<R45><C3><FROM><R45><C80><LINE><||3>" SKIP  
            "<R40><C3><FROM><R45><C3><LINE><||3>" SKIP
            "<R40><C80><FROM><R45><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=5><R-2><B><p14> Customer Name </B>" SKIP.
        PUT "<FArial><=5><R+1><B><P27>  " tt-word-print.cust-name FORMAT "x(25)"  "</B>" SKIP.
        
        PUT "<R48><C3><#6><FROM><R48><C80><RECT><||3>" SKIP
            "<R52><C3><FROM><R52><C80><LINE><||3>" SKIP  
            "<R48><C3><FROM><R52><C3><LINE><||3>" SKIP
            "<R48><C80><FROM><R52><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=6><R-2><B><p14> Customer PO# </B>" SKIP.
        PUT "<FArial><=6><R49><C4><B><P24>  " tt-word-print.cust-po-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<R55><C3><#7><FROM><R55><C80><RECT><||3>" SKIP
            "<R59><C3><FROM><R59><C80><LINE><||3>" SKIP  
            "<R55><C3><FROM><R59><C3><LINE><||3>" SKIP
            "<R55><C80><FROM><R59><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=7><R-2><B><P14> Item Name </B>" SKIP.
        PUT "<FArial><=7><R55.5><C2><B><P30>  " tt-word-print.i-name FORMAT "x(25)"  "</B>" SKIP.
        
        PUT "<R62><C3><#8><FROM><R62><C80><RECT><||3>" SKIP
            "<R68><C3><FROM><R68><C80><LINE><||3>" SKIP  
            "<R62><C3><FROM><R68><C3><LINE><||3>" SKIP
            "<R62><C80><FROM><R68><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=8><R-2><B><P14> Customer Part# </B>" SKIP.
        PUT "<FArial><=8><R63.3><C2><B><P30>  " tt-word-print.cust-part-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<R71><C3><#9><FROM><R71><C80><RECT><||3>" SKIP
            "<R75><C3><FROM><R75><C80><LINE><||3>" SKIP  
            "<R71><C3><FROM><R75><C3><LINE><||3>" SKIP
            "<R71><C80><FROM><R75><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=9><R-2><B><P14> Finished Goods Item# </B>" SKIP.
        PUT "<FArial><=9><R71.5><C2><B><P30>  " tt-word-print.i-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<R78><C3><#10><FROM><R78><C20><RECT><||3>" SKIP
            "<R83><C3><FROM><R83><C20><LINE><||3>" SKIP  
            "<R78><C3><FROM><R83><C3><LINE><||3>" SKIP
            "<R78><C20><FROM><R83><C20><LINE><||3>" SKIP.
        
        PUT "<FArial><=10><R76><C4><B><P22> Qty/Pallet </B>" SKIP.
        PUT "<FArial><=10><R79><C4><B><P24>  " tt-word-print.total-unit FORMAT ">,>>>,>>9" "</B>" SKIP.
        
        PUT "<R78><C23><#11><FROM><R78><C48><RECT><||3>" SKIP
            "<R83><C23><FROM><R83><C48><LINE><||3>" SKIP  
            "<R78><C23><FROM><R83><C23><LINE><||3>" SKIP
            "<R78><C48><FROM><R83><C48><LINE><||3>" SKIP.
        
        PUT "<FArial><=10><R76><C22><B><P22> Date</B>" SKIP.
        PUT "<FArial><=10><R79><C25><B><P24>  " tt-word-print.due-date FORMAT "99/99/9999" "</B>" SKIP.
        
        PUT "<R78><C51><#14><FROM><R78><C80><RECT><||3>" SKIP
            "<R83><C51><FROM><R83><C80><LINE><||3>" SKIP  
            "<R78><C51><FROM><R83><C51><LINE><||3>" SKIP
            "<R78><C80><FROM><R83><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=10><R76><C50><B><P22> Job# </B>" SKIP.
        PUT "<FArial><=10><R79><C55><B><P24>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99")  "</B><P12>" SKIP.
        .
        
        PUT "<FCourier New>".


END.  /* else do */







