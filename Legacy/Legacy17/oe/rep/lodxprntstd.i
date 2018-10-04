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


IF tb_print-view THEN DO:
    PUT  "<FArial>".
    PUT  "<C+25><#1>".
    PUT  "<=1>" SKIP.
    PUT  "<C1><#2>".
    PUT "<FArial><=4><R11><C5><P12> " "......................................................................  Fold Here  ......................................................................" "" SKIP(2).
    
    PUT   "<#=100><AT=2.10,1.5><FROM><AT=+.6,+6><BARCODE,TYPE=128B,ANGLE=180,CHECKSUM=NONE,VALUE=" tt-word-print.tag-no FORMAT "x(20)"  ">"
        "<AT=2.9,3.2><P16>" tt-word-print.tag-no FORMAT "x(20)"  .
    
    PUT  SKIP(3) "<R21><C5><FROM><R21><C80><LINE>" SKIP.
    
    PUT  "<R24><C39><FROM><R32><C39><LINE>" SKIP.
    
    PUT "<||><R23><C5><#4><FROM><R33><C70><RECT>" SKIP.     
    
    PUT "<FArial><=4><R25><C6><P16><B>" company.NAME FORMAT "x(25)"  "</B>" SKIP.
    PUT "<FArial><=4><R25><C40><P14> Phone: " cPhone FORMAT "x(15)"  "" SKIP(2).
    PUT "<FArial><=4><R27><C6><P14>" company.addr[1] FORMAT "x(30)" "" .
    PUT "<FArial><=4><R27><C40><P14> Fax: " cFax FORMAT "x(15)" "" SKIP(3).
    PUT "<FArial><=4><R29><C6><P14>" STRING(company.city,"x(15)") " " company.state FORMAT "x(2)" " " company.zip "" .
    PUT "<FArial><=4><R29><C40><P14> " cEmail FORMAT "x(30)"  "" SKIP(2).
    
    PUT "<||><R34><C5><#4><FROM><R47><C70><RECT>" SKIP. 
    
    PUT "<FArial><=5><R35><C6><P16> Customer:    " "<B><C20>" tt-word-print.cust-name FORMAT "x(27)" "</B>"   "" SKIP.
    PUT "<FArial><=5><R37><C6><P16> PO#:            " "<B><C20>" tt-word-print.cust-po-no FORMAT "x(15)" "</B>"   "" SKIP.
    PUT "<FArial><=5><R39><C6><P16> Job#:            " "<B><C20>" tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99") "</B>"   "" SKIP.
    PUT "<FArial><=5><R41><C6><P16> Item Name:  " "<B><C20>" tt-word-print.i-name FORMAT "x(27)" "</B>"   "" SKIP.
    PUT "<FArial><=5><R43><C6><P16> Part#:           " "<B><C20>" tt-word-print.cust-part-no FORMAT "x(15)" "</B>"   "" SKIP.
    PUT "<FArial><=5><R45><C6><P16> FG#:             " "<B><C20>" tt-word-print.i-no FORMAT "x(15)" "</B>"   "" SKIP.
    
    PUT  "<R21><C71><FROM><R64><C71><LINE>" SKIP.
    
    PUT   "<#=100><AT=4.08,7.50><FROM><AT=+.6,+6><BARCODE,TYPE=128B,ANGLE=90,CHECKSUM=NONE,VALUE=" tt-word-print.tag-no FORMAT "x(20)"  ">".
    /*    "<AT=6.00,7.35><angle=270>" tt-word-print.tag-no FORMAT "x(20)" "</angle>"  .*/
     
    PUT "<FArial><=6><R49><C5><P16> Qty/Case " SKIP.
    PUT "<FArial><=6><R51><C5><B><P30>  " tt-word-print.pcs "</B>" SKIP.
    PUT "<FArial><=6><R49><C22><P16> Case/Pallett " SKIP.
    PUT "<FArial><=6><R51><C20><B><P30>  " tt-word-print.bundle "</B>" SKIP.
    PUT "<FArial><=6><R49><C39><P16> Partial Case " SKIP.
    PUT "<FArial><=6><R51><C36><B><P30>  " tt-word-print.partial "</B>" SKIP.
    PUT "<FArial><=6><R49><C56><P16> Qty/Pallett " SKIP.
    PUT "<FArial><=6><R51><C54><B><P30>  " tt-word-print.total-unit "</B>" SKIP.
    
    PUT "<FArial><=7><R58><C5><P16> Weight Case " SKIP.
    PUT "<FArial><=7><R60><C6><B><P30>  " tt-word-print.case-wt "</B>" SKIP.
    
    
    PUT  "<R56><C22><FROM><R56><C70><LINE>" SKIP.
    PUT  "<R56><C22><FROM><R64><C22><LINE>" SKIP.
    
    PUT "<FArial><=8><R57><C24><P14> Counted By: " SKIP.
    
    PUT "<||><R57><C36><#8><FROM><R59><C51><RECT>" SKIP. 
    
    PUT "<FArial><=9><R57><C53><P14> Date: " SKIP.
    
    PUT "<||><R57><C60><#9><FROM><R59><C70><RECT>" SKIP. 
    
    PUT "<FArial><=10><R60><C24><P14> Checked By " SKIP.
    
    PUT "<||><R60><C36><#10><FROM><R62><C51><RECT>" SKIP. 
    
    PUT "<FArial><=11><R60><C53><P14> Date: " SKIP.
    
    PUT "<||><R60><C60><#11><FROM><R62><C70><RECT>" SKIP. 
    .
    
    PUT "<FCourier New>".

END.
ELSE DO:


        
        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        PUT "<FArial><=4><R24><C5><P12> " "......................................................................  Fold Here  ......................................................................" "" SKIP(2).
        
        PUT   "<#=100><AT=4.30,1.5><FROM><AT=+.6,+6><BARCODE,TYPE=128B,ANGLE=180,CHECKSUM=NONE,VALUE=" tt-word-print.tag-no FORMAT "x(20)"  ">"
            "<AT=5.06,3.2><P16>" tt-word-print.tag-no FORMAT "x(20)"  .
        
        PUT  SKIP(3) "<R34><C5><FROM><R34><C80><LINE>" SKIP.
        
        PUT  "<R37><C39><FROM><R45><C39><LINE>" SKIP.
        
        PUT "<||><R36><C5><#4><FROM><R46><C70><RECT>" SKIP. 
        
        
        PUT "<FArial><=4><R38><C6><P16><B>" company.NAME FORMAT "x(25)"  "</B>" SKIP.
        PUT "<FArial><=4><R38><C40><P14> Phone: " cPhone FORMAT "x(15)"  "" SKIP(2).
        PUT "<FArial><=4><R40><C6><P14>" company.addr[1] FORMAT "x(30)" "" .
        PUT "<FArial><=4><R40><C40><P14> Fax: " cFax FORMAT "x(15)" "" SKIP(3).
        PUT "<FArial><=4><R42><C6><P14>" STRING(company.city,"x(15)") " " company.state FORMAT "x(2)" " " company.zip "" .
        PUT "<FArial><=4><R42><C40><P14> " cEmail FORMAT "x(30)"  "" SKIP(2).
        
              
        PUT "<||><R47><C5><#4><FROM><R60><C70><RECT>" SKIP. 
        
        PUT "<FArial><=5><R48><C6><P16> Customer:    " "<B><C20>" tt-word-print.cust-name FORMAT "x(27)" "</B>"   "" SKIP.
        PUT "<FArial><=5><R50><C6><P16> PO#:            " "<B><C20>" tt-word-print.cust-po-no FORMAT "x(15)" "</B>"   "" SKIP.
        PUT "<FArial><=5><R52><C6><P16> Job#:            " "<B><C20>" tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99") "</B>"   "" SKIP.
        PUT "<FArial><=5><R54><C6><P16> Item Name:  " "<B><C20>" tt-word-print.i-name FORMAT "x(27)" "</B>"   "" SKIP.
        PUT "<FArial><=5><R56><C6><P16> Part#:           " "<B><C20>" tt-word-print.cust-part-no FORMAT "x(15)" "</B>"   "" SKIP.
        PUT "<FArial><=5><R58><C6><P16> FG#:             " "<B><C20>" tt-word-print.i-no FORMAT "x(15)" "</B>"   "" SKIP.
        
        PUT  "<R34><C71><FROM><R77><C71><LINE>" SKIP.
        
        PUT   "<#=100><AT=6.20,7.50><FROM><AT=+.6,+6><BARCODE,TYPE=128B,ANGLE=90,CHECKSUM=NONE,VALUE=" tt-word-print.tag-no FORMAT "x(20)"  ">".
        /*    "<AT=6.00,7.35><angle=270>" tt-word-print.tag-no FORMAT "x(20)" "</angle>"  .*/
         
        PUT "<FArial><=6><R62><C5><P16> Qty/Case " SKIP.
        PUT "<FArial><=6><R64><C5><B><P30>  " tt-word-print.pcs "</B>" SKIP.
        PUT "<FArial><=6><R62><C22><P16> Case/Pallett " SKIP.
        PUT "<FArial><=6><R64><C20><B><P30>  " tt-word-print.bundle "</B>" SKIP.
        PUT "<FArial><=6><R62><C39><P16> Partial Case " SKIP.
        PUT "<FArial><=6><R64><C36><B><P30>  " tt-word-print.partial "</B>" SKIP.
        PUT "<FArial><=6><R62><C56><P16> Qty/Pallett " SKIP.
        PUT "<FArial><=6><R64><C54><B><P30>  " tt-word-print.total-unit "</B>" SKIP.
        
        PUT "<FArial><=7><R71><C5><P16> Weight Case " SKIP.
        PUT "<FArial><=7><R73><C6><B><P30>  " tt-word-print.case-wt "</B>" SKIP.
        
        
        PUT  "<R69><C22><FROM><R69><C70><LINE>" SKIP.
        PUT  "<R69><C22><FROM><R77><C22><LINE>" SKIP.
        
        PUT "<FArial><=8><R70><C24><P14> Counted By: " SKIP.       
      
        PUT "<||><R70><C36><#8><FROM><R72><C51><RECT>" SKIP. 
        
        PUT "<FArial><=9><R70><C53><P14> Date: " SKIP.        
       

        PUT "<||><R70><C60><#9><FROM><R72><C70><RECT>" SKIP. 
        
        PUT "<FArial><=10><R73><C24><P14> Checked By " SKIP.       
       

        PUT "<||><R73><C36><#10><FROM><R75><C51><RECT>" SKIP. 
        
        PUT "<FArial><=11><R73><C53><P14> Date: " SKIP.       
        

         PUT "<||><R73><C60><#11><FROM><R75><C70><RECT>" SKIP. 
        .
        
        PUT "<FCourier New>".
END. /* else do*/







