/* oe/rep/invxprnt.i */  

FIND FIRST cust NO-LOCK
     WHERE cust.company EQ cocode
       AND cust.cust-no EQ tt-word-print.cust-no NO-ERROR .
 IF AVAIL cust THEN DO:
     ASSIGN
       cEmail = cust.email .
       cPhone = string(cust.area-code,"(xxx)") +  string(cust.phone,"xxx-xxxx") .
       cFax   = STRING(SUBSTR(cust.fax,1,3),"(xxx)") + "-" + STRING(SUBSTR(cust.fax,4),"xxx-xxxx")   .
 END.
     FIND FIRST company NO-LOCK
          WHERE company.company  = cocode  NO-ERROR. 

IF tb_print-view THEN DO:
        
        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        PUT UNFORMATTED   
               "<C4><R5.5><#1><R+10><C+70><IMAGE#1=" ls-full-img1.
        
         PUT "<R17><C3><P25><FROM><R17><C80><LINE>" SKIP.
        
         PUT "<||><R20><C3><#4><FROM><R25><C80><RECT>" SKIP.            
        
        PUT "<FArial><=4><R18><C3><B><p15> Customer Name </B>" SKIP.
        PUT "<FArial><=4><R21.2><C3><B><P24>  " tt-word-print.cust-name FORMAT "x(30)" "</B>" SKIP.
        
        PUT "<||><R28><C3><#5><FROM><R33><C80><RECT>" SKIP.            
        
        PUT "<FArial><=5><R-2><B><p15> PO#                                                                Customer Part# </B>" SKIP.
        PUT "<FArial><=5><R29><C3><B><P23>  " tt-word-print.cust-po-no FORMAT "x(15)"  "</B>" SKIP.
        PUT "<FArial><=5><R29><C43><B><P26>  " tt-word-print.cust-part-no FORMAT "x(15)" "</B>" SKIP.
        
        PUT "<||><R36><C3><#5><FROM><R41><C80><RECT>" SKIP.            
        
        PUT "<FArial><=5><R-2><B><p15> Item Name</B>" SKIP.
        PUT "<FArial><=5><R37><C3><B><P26>  " tt-word-print.i-name FORMAT "x(30)" "</B>" SKIP.
        
        PUT "<||><R44><C3><#5><FROM><R50><C80><RECT>" SKIP.            
        
        PUT "<FArial><=5><R-2><B><p15> Finished Goods Item#</B>" SKIP.
        PUT "<FArial><=5><R45.5><C3><B><P26>  " tt-word-print.i-no FORMAT "x(15)" "</B>" SKIP.
        
        
        PUT "<||><R55><C3><#10><FROM><R60><C18><RECT>" SKIP.            
        
        PUT "<FArial><=10><R53><C4><B><P22> Qty </B>" SKIP.
        PUT "<FArial><=10><R56><C2><B><P21>" tt-word-print.ord-qty FORMAT "->,>>>,>>9"  "</B>" SKIP.
        
        PUT "<||><R55><C20><#11><FROM><R60><C29><RECT>" SKIP.            
        
        PUT "<FArial><=11><R53><C24><B><P26> PALLET </B>" SKIP.
        PUT "<FArial><=11><R56><C19><B><P24>  " tt-word-print.pcs "</B>" SKIP.
        PUT "<FArial><=11><R56><C29><B><P26> of </B>" .
        
        PUT "<||><R55><C35><#12><FROM><R60><C43><RECT>" SKIP.
            
        PUT "<FArial><=12><R56><C33.8><B><P24>  " tt-word-print.pcs "</B>" SKIP.
        
        
        PUT "<||><R55><C45><#13><FROM><R60><C62><RECT>" SKIP.            
        
        PUT "<FArial><=10><R53><C47><B><P22> Job# </B>" SKIP.
        PUT "<FArial><=10><R56><C44><B><P24>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99") "</B>" SKIP.
        
        PUT "<||><R55><C64><#14><FROM><R60><C80><RECT>" SKIP.
                    
        PUT "<FArial><=10><R53><C65><B><P22> Date </B>" SKIP.
        PUT "<FArial><=10><R56><C64><B><P24>  " tt-word-print.due-date FORMAT "99/99/99"  "</B>" SKIP.        
        
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
        
         PUT "<R37><C3><P25><FROM><R37><C80><LINE>" SKIP.
        
         PUT "<||><R40><C3><#4><FROM><R45><C80><RECT>" SKIP.            
        
        PUT "<FArial><=4><R38><C3><B><p15> Customer Name </B>" SKIP.
        PUT "<FArial><=4><R41.2><C3><B><P24>  " tt-word-print.cust-name FORMAT "x(30)" "</B>" SKIP.
        
        PUT "<||><R48><C3><#5><FROM><R53><C80><RECT>" SKIP.            
        
        PUT "<FArial><=5><R-2><B><p15> PO#                                                                Customer Part# </B>" SKIP.
        PUT "<FArial><=5><R49><C3><B><P23>  " tt-word-print.cust-po-no FORMAT "x(15)"  "</B>" SKIP.
        PUT "<FArial><=5><R49><C43><B><P26>  " tt-word-print.cust-part-no FORMAT "x(15)" "</B>" SKIP.
        
        PUT "<||><R56><C3><#5><FROM><R61><C80><RECT>" SKIP.            
        
        PUT "<FArial><=5><R-2><B><p15> Item Name</B>" SKIP.
        PUT "<FArial><=5><R57><C3><B><P26>  " tt-word-print.i-name FORMAT "x(30)" "</B>" SKIP.
        
        PUT "<||><R64><C3><#5><FROM><R70><C80><RECT>" SKIP.            
        
        PUT "<FArial><=5><R-2><B><p15> Finished Goods Item#</B>" SKIP.
        PUT "<FArial><=5><R65.5><C3><B><P26>  " tt-word-print.i-no FORMAT "x(15)" "</B>" SKIP.
        
        
        PUT "<||><R75><C3><#10><FROM><R80><C18><RECT>" SKIP.            
        
        PUT "<FArial><=10><R73><C4><B><P22> Qty </B>" SKIP.
        PUT "<FArial><=10><R76><C2><B><P21>" tt-word-print.ord-qty FORMAT "->,>>>,>>9"  "</B>" SKIP.
        
        PUT "<||><R75><C20><#11><FROM><R80><C29><RECT>" SKIP.            
        
        PUT "<FArial><=11><R73><C24><B><P26> PALLET </B>" SKIP.
        PUT "<FArial><=11><R76><C19><B><P24>  " tt-word-print.pcs "</B>" SKIP.
        PUT "<FArial><=11><R76><C29><B><P26> of </B>" .
        
        PUT "<||><R75><C35><#12><FROM><R80><C43><RECT>" SKIP.
            
        PUT "<FArial><=12><R76><C33.8><B><P24>  " tt-word-print.pcs "</B>" SKIP.        
        
        PUT "<||><R75><C45><#13><FROM><R80><C62><RECT>" SKIP.            
        
        PUT "<FArial><=10><R73><C47><B><P22> Job# </B>" SKIP.
        PUT "<FArial><=10><R76><C44><B><P24>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99") "</B>" SKIP.
        
        PUT "<||><R75><C64><#14><FROM><R80><C80><RECT>" SKIP.            
        
        PUT "<FArial><=10><R73><C65><B><P22> Date </B>" SKIP.
        PUT "<FArial><=10><R76><C64><B><P24>  " tt-word-print.due-date FORMAT "99/99/99"  "</B>" SKIP.
        
        
         .
        
        PUT "<FCourier New>".
END. /* else do */







