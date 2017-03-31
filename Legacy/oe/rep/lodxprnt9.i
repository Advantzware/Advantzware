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
        PUT UNFORMATTED   
               "<C4><R4.5><#1><R+10><C+70><IMAGE#1=" ls-full-img1.
        PUT "<R17><C3><P25><FROM><R17><C80><LINE>" SKIP.
        
         PUT "<FArial><R15><C3><p12><B>" tt-word-print.ship-add1 FORMAT "x(30)"  "</B>" .
         PUT "<FArial><=4><R15><C27><p11><B>" STRING(tt-word-print.ship-city,"x(15)") " " tt-word-print.ship-state FORMAT "x(2)" " " tt-word-print.ship-zip "</B>" .
        
         PUT "<FArial><=4><R15><C44><p12><B>        Phone: " cPhone FORMAT "x(15)"  "</B>" .
         PUT "<FArial><=4><R15><C64><p12><B>   Fax: " cFax FORMAT "x(15)"  "</B>" SKIP.
        
        PUT "<R20><C3><#5><FROM><R20><C80><RECT><||3>" SKIP
            "<R25><C3><FROM><R25><C80><LINE><||3>" SKIP  
            "<R20><C3><FROM><R25><C3><LINE><||3>" SKIP
            "<R20><C80><FROM><R25><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=5><R-2><B><p14> Customer Name                                                Ship To:</B>" SKIP.
        PUT "<FArial><=5><R21.6><B><P15>  " tt-word-print.cust-name FORMAT "x(27)"  "</B>" SKIP.
        PUT "<FArial><=5><R21.6><C43><B><P15>  " tt-word-print.ship-name FORMAT "x(27)"    "</B>" SKIP.
        
        PUT "<R28><C3><#6><FROM><R28><C80><RECT><||3>" SKIP
            "<R32><C3><FROM><R32><C80><LINE><||3>" SKIP  
            "<R28><C3><FROM><R32><C3><LINE><||3>" SKIP
            "<R28><C80><FROM><R32><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=6><R-2><B><p14> Customer PO# / Job# </B>" SKIP.
        PUT "<FArial><=6><R29><C4><B><P24>  " tt-word-print.cust-po-no FORMAT "x(15)" "          "  tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99")    "</B>" SKIP.
        
        PUT "<R35><C3><#7><FROM><R35><C80><RECT><||3>" SKIP
            "<R39><C3><FROM><R39><C80><LINE><||3>" SKIP  
            "<R35><C3><FROM><R39><C3><LINE><||3>" SKIP
            "<R35><C80><FROM><R39><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=7><R-2><B><P14> Item Name </B>" SKIP.
        PUT "<FArial><=7><R35.5><C2><B><P30>  " tt-word-print.i-name FORMAT "x(25)"   "</B>" SKIP.
        
        PUT "<R42><C3><#8><FROM><R42><C80><RECT><||3>" SKIP
            "<R48><C3><FROM><R48><C80><LINE><||3>" SKIP  
            "<R42><C3><FROM><R48><C3><LINE><||3>" SKIP
            "<R42><C80><FROM><R48><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=8><R-2><B><P14> Customer Part# </B>" SKIP.
        PUT "<FArial><=8><R43.4><C2><B><P30>  " tt-word-print.cust-part-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<R51><C3><#9><FROM><R51><C80><RECT><||3>" SKIP
            "<R55><C3><FROM><R55><C80><LINE><||3>" SKIP  
            "<R51><C3><FROM><R55><C3><LINE><||3>" SKIP
            "<R51><C80><FROM><R55><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=9><R-2><B><P14> Finished Goods Item# </B>" SKIP.
        PUT "<FArial><=9><R51.5><C2><B><P30>  " tt-word-print.i-no FORMAT "x(15)"    "</B>" SKIP.
        
        PUT "<R58><C3><#10><FROM><R58><C18><RECT><||3>" SKIP
            "<R63><C3><FROM><R63><C18><LINE><||3>" SKIP  
            "<R58><C3><FROM><R63><C3><LINE><||3>" SKIP
            "<R58><C18><FROM><R63><C18><LINE><||3>" SKIP.
        
        PUT "<FArial><=10><R56><C4><B><P22> Qty/Pallet </B>" SKIP.
        PUT "<FArial><=10><R59><C2><B><P24>  " tt-word-print.total-unit FORMAT ">,>>>,>>9" "</B>" SKIP.
        
        PUT "<R58><C20><#11><FROM><R58><C29><RECT><||3>" SKIP
            "<R63><C20><FROM><R63><C29><LINE><||3>" SKIP  
            "<R58><C20><FROM><R63><C20><LINE><||3>" SKIP
            "<R58><C29><FROM><R63><C29><LINE><||3>" SKIP.
        
        PUT "<FArial><=11><R56><C24><B><P26> PALLET </B>" SKIP.
        PUT "<FArial><=11><R59><C19><B><P24>  " tt-word-print.pcs "</B>" SKIP.
        
        PUT "<FArial><=11><R59><C29><B><P26> of </B>" .
        PUT "<R58><C35><#12><FROM><R58><C43><RECT><||3>" SKIP
            "<R63><C35><FROM><R63><C43><LINE><||3>" SKIP  
            "<R58><C35><FROM><R63><C35><LINE><||3>" SKIP
            "<R58><C43><FROM><R63><C43><LINE><||3>" SKIP.
        PUT "<FArial><=12><R59><C33.8><B><P24>  " tt-word-print.pcs "</B>" SKIP.
        
        
        PUT "<R58><C45><#13><FROM><R58><C62><RECT><||3>" SKIP
            "<R63><C45><FROM><R63><C62><LINE><||3>" SKIP  
            "<R58><C45><FROM><R63><C45><LINE><||3>" SKIP
            "<R58><C62><FROM><R63><C62><LINE><||3>" SKIP.
        
        PUT "<FArial><=10><R56><C47><B><P22> Job# </B>" SKIP.
        PUT "<FArial><=10><R59><C44><B><P24>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99")  "</B>" SKIP.
        
        PUT "<R58><C64><#14><FROM><R58><C80><RECT><||3>" SKIP
            "<R63><C64><FROM><R63><C80><LINE><||3>" SKIP  
            "<R58><C64><FROM><R63><C64><LINE><||3>" SKIP
            "<R58><C80><FROM><R63><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=10><R56><C65><B><P22> Date </B>" SKIP.
        PUT "<FArial><=10><R59><C64><B><P24>  " tt-word-print.due-date FORMAT "99/99/99"  "</B><P12>" SKIP.
        .
        
        PUT "<FCourier New>".
END.
ELSE DO:

        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        PUT UNFORMATTED   
               "<C4><R24.5><#1><R+10><C+70><IMAGE#1=" ls-full-img1.
        PUT "<R37><C3><P25><FROM><R37><C80><LINE>" SKIP.
        
         PUT "<FArial><R35><C3><p12><B>" tt-word-print.ship-add1 FORMAT "x(30)"  "</B>" .
         PUT "<FArial><=4><R35><C27><p11><B>" STRING(tt-word-print.ship-city,"x(15)") " " tt-word-print.ship-state FORMAT "x(2)" " " tt-word-print.ship-zip "</B>" .
        
         PUT "<FArial><=4><R35><C44><p12><B>        Phone: " cPhone FORMAT "x(15)"  "</B>" .
         PUT "<FArial><=4><R35><C64><p12><B>   Fax: " cFax FORMAT "x(15)"  "</B>" SKIP.
        
        PUT "<R40><C3><#5><FROM><R40><C80><RECT><||3>" SKIP
            "<R45><C3><FROM><R45><C80><LINE><||3>" SKIP  
            "<R40><C3><FROM><R45><C3><LINE><||3>" SKIP
            "<R40><C80><FROM><R45><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=5><R-2><B><p14> Customer Name                                                Ship To:</B>" SKIP.
        PUT "<FArial><=5><R41.6><B><P15>  " tt-word-print.cust-name FORMAT "x(27)"  "</B>" SKIP.
        PUT "<FArial><=5><R41.6><C43><B><P15>  " tt-word-print.ship-name FORMAT "x(27)"    "</B>" SKIP.
        
        PUT "<R48><C3><#6><FROM><R48><C80><RECT><||3>" SKIP
            "<R52><C3><FROM><R52><C80><LINE><||3>" SKIP  
            "<R48><C3><FROM><R52><C3><LINE><||3>" SKIP
            "<R48><C80><FROM><R52><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=6><R-2><B><p14> Customer PO# / Job# </B>" SKIP.
        PUT "<FArial><=6><R49><C4><B><P24>  " tt-word-print.cust-po-no FORMAT "x(15)" "          "  tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99")    "</B>" SKIP.
        
        PUT "<R55><C3><#7><FROM><R55><C80><RECT><||3>" SKIP
            "<R59><C3><FROM><R59><C80><LINE><||3>" SKIP  
            "<R55><C3><FROM><R59><C3><LINE><||3>" SKIP
            "<R55><C80><FROM><R59><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=7><R-2><B><P14> Item Name </B>" SKIP.
        PUT "<FArial><=7><R55.5><C2><B><P30>  " tt-word-print.i-name FORMAT "x(25)"   "</B>" SKIP.
        
        PUT "<R62><C3><#8><FROM><R62><C80><RECT><||3>" SKIP
            "<R68><C3><FROM><R68><C80><LINE><||3>" SKIP  
            "<R62><C3><FROM><R68><C3><LINE><||3>" SKIP
            "<R62><C80><FROM><R68><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=8><R-2><B><P14> Customer Part# </B>" SKIP.
        PUT "<FArial><=8><R63.4><C2><B><P30>  " tt-word-print.cust-part-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<R71><C3><#9><FROM><R71><C80><RECT><||3>" SKIP
            "<R75><C3><FROM><R75><C80><LINE><||3>" SKIP  
            "<R71><C3><FROM><R75><C3><LINE><||3>" SKIP
            "<R71><C80><FROM><R75><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=9><R-2><B><P14> Finished Goods Item# </B>" SKIP.
        PUT "<FArial><=9><R71.5><C2><B><P30>  " tt-word-print.i-no FORMAT "x(15)"    "</B>" SKIP.
        
        PUT "<R78><C3><#10><FROM><R78><C18><RECT><||3>" SKIP
            "<R83><C3><FROM><R83><C18><LINE><||3>" SKIP  
            "<R78><C3><FROM><R83><C3><LINE><||3>" SKIP
            "<R78><C18><FROM><R83><C18><LINE><||3>" SKIP.
        
        PUT "<FArial><=10><R76><C4><B><P22> Qty/Pallet </B>" SKIP.
        PUT "<FArial><=10><R79><C2><B><P24>  " tt-word-print.total-unit FORMAT ">,>>>,>>9" "</B>" SKIP.
        
        PUT "<R78><C20><#11><FROM><R78><C29><RECT><||3>" SKIP
            "<R83><C20><FROM><R83><C29><LINE><||3>" SKIP  
            "<R78><C20><FROM><R83><C20><LINE><||3>" SKIP
            "<R78><C29><FROM><R83><C29><LINE><||3>" SKIP.
        
        PUT "<FArial><=11><R76><C24><B><P26> PALLET </B>" SKIP.
        PUT "<FArial><=11><R79><C19><B><P24>  " tt-word-print.pcs "</B>" SKIP.
        
        PUT "<FArial><=11><R79><C29><B><P26> of </B>" .
        PUT "<R78><C35><#12><FROM><R78><C43><RECT><||3>" SKIP
            "<R83><C35><FROM><R83><C43><LINE><||3>" SKIP  
            "<R78><C35><FROM><R83><C35><LINE><||3>" SKIP
            "<R78><C43><FROM><R83><C43><LINE><||3>" SKIP.
        PUT "<FArial><=12><R79><C33.8><B><P24>  " tt-word-print.pcs "</B>" SKIP.
        
        
        PUT "<R78><C45><#13><FROM><R78><C62><RECT><||3>" SKIP
            "<R83><C45><FROM><R83><C62><LINE><||3>" SKIP  
            "<R78><C45><FROM><R83><C45><LINE><||3>" SKIP
            "<R78><C62><FROM><R83><C62><LINE><||3>" SKIP.
        
        PUT "<FArial><=10><R76><C47><B><P22> Job# </B>" SKIP.
        PUT "<FArial><=10><R79><C44><B><P24>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99")  "</B>" SKIP.
        
        PUT "<R78><C64><#14><FROM><R78><C80><RECT><||3>" SKIP
            "<R83><C64><FROM><R83><C80><LINE><||3>" SKIP  
            "<R78><C64><FROM><R83><C64><LINE><||3>" SKIP
            "<R78><C80><FROM><R83><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=10><R76><C65><B><P22> Date </B>" SKIP.
        PUT "<FArial><=10><R79><C64><B><P24>  " tt-word-print.due-date FORMAT "99/99/99"  "</B><P12>" SKIP.
        .
        
        PUT "<FCourier New>".
END. /* else do */







