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
        PUT   "<#=100><AT=,2><FROM><AT=+.8,+4><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.tag-no FORMAT "x(20)" ">"
            "<AT=,3><P14>" tt-word-print.tag-no FORMAT "x(20)" .
        
        PUT  SKIP(3) "<R9><C3><FROM><R9><C80><LINE>" SKIP .
        
        PUT "<R11><C3><#4><FROM><R11><C80><RECT><||3>" SKIP
            "<R18><C3><FROM><R18><C80><LINE><||3>" SKIP  
            "<R11><C3><FROM><R18><C3><LINE><||3>" SKIP
            "<R11><C80><FROM><R18><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=4><R+1><C5><B>" company.name FORMAT "x(25)"  "</B>" SKIP.
        PUT "<FArial><=4><R+1><C40><B> Email: " cEmail FORMAT "x(30)"  "</B>" SKIP(2).
        PUT "<FArial><=4><R+3><C5><B>" company.addr[1] FORMAT "x(25)" "</B>" .
        PUT "<FArial><=4><R+3><C40><B> Phone: " cPhone FORMAT "x(15)" "</B>" SKIP(2).
        PUT "<FArial><=4><R+5><C5><B>" STRING(company.city,"x(15)") " " company.state FORMAT "x(2)" " " company.zip  "</B>" .
        PUT "<FArial><=4><R+5><C40><B> Fax: " cFax FORMAT "x(15)" "</B>" SKIP(2).
        
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
        
        PUT "<R46><C3><#11><FROM><R46><C19><RECT><||3>"
            "<R50><C3><FROM><R50><C19><LINE><||3>"  
            "<R46><C3><FROM><R50><C3><LINE><||3>" 
            "<R46><C19><FROM><R50><C19><LINE><||3>" .
        
        PUT "<FArial><=11><R-2><B><P14> Qty / Case</B>" SKIP.
        PUT "<FArial><=11><R46.5><B><C4><P30>  " tt-word-print.pcs   "</B>" SKIP.
        
        PUT "<R46><C23><#12><FROM><R46><C38><RECT><||3>" 
            "<R50><C23><FROM><R50><C38><LINE><||3>"  
            "<R46><C23><FROM><R50><C23><LINE><||3>" 
            "<R46><C38><FROM><R50><C38><LINE><||3>" .
        
        PUT "<FArial><=12><R-2><B><P14> Cases / Pallet</B>" SKIP.
        PUT "<FArial><=12><R46.5><B><C24><P30>  "  tt-word-print.bundle "</B>" SKIP.
        
        PUT "<R46><C42><#13><FROM><R46><C59><RECT><||3>" 
            "<R50><C42><FROM><R50><C59><LINE><||3>"   
            "<R46><C42><FROM><R50><C42><LINE><||3>" 
            "<R46><C59><FROM><R50><C59><LINE><||3>" .
        PUT "<FArial><=13><R-2><B><P14> Partial Case</B>" SKIP.
        PUT "<FArial><=13><R46.5><B><C43><P30>  "  tt-word-print.partial "</B>" SKIP.
        
        PUT "<R46><C63><#14><FROM><R46><C80><RECT><||3>" 
            "<R50><C63><FROM><R50><C80><LINE><||3>"  
            "<R46><C63><FROM><R50><C63><LINE><||3>" 
            "<R46><C80><FROM><R50><C80><LINE><||3>" .
        
        PUT "<FArial><=14><R-2><B><P14> Qty Pallet</B>" SKIP.
        PUT "<FArial><=14><R46.5><B><C64><P30>  "  tt-word-print.total-unit  "</B>" SKIP.
        
        PUT "<R53><C3><#15><FROM><R53><C18><RECT><||3>" 
            "<R57><C3><FROM><R57><C18><LINE><||3>"  
            "<R53><C3><FROM><R57><C3><LINE><||3>" 
            "<R53><C18><FROM><R57><C18><LINE><||3>".
        
        PUT "<FArial><=15><R-2><B><P14> Wt / Case</B>" SKIP.
        PUT "<FArial><=15><R53.5><B><C4><P30>  " tt-word-print.case-wt "</B>" SKIP.
        
        PUT "<R53><C22><#16><FROM><R53><C48><RECT><||3>" 
            "<R57><C22><FROM><R57><C48><LINE><||3>"   
            "<R53><C22><FROM><R57><C22><LINE><||3>" 
            "<R53><C48><FROM><R57><C48><LINE><||3>" .
        
        PUT "<FCourier New><=16><R+4><C27><P14>Counted By" SKIP.
        
        PUT "<R53><C50><#17><FROM><R53><C80><RECT><||3>" 
            "<R57><C50><FROM><R57><C80><LINE><||3>"   
            "<R53><C50><FROM><R57><C50><LINE><||3>" 
            "<R53><C80><FROM><R57><C80><LINE><||3>" .
        
        PUT "<FCourier New><=17><R+4><C57>Checked By" SKIP.
        .
        
        PUT "<FCourier New>".

  END.  /* if tb_print-view */
  ELSE do: 
        
        
        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        PUT   "<#=100><AT=3.70,2><FROM><AT=+.8,+4><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.tag-no FORMAT "x(20)" ">"
            "<AT=4.60,3><P14>" tt-word-print.tag-no FORMAT "x(20)" .
        
        PUT  SKIP(3) "<R31><C3><FROM><R31><C80><LINE>" SKIP .
        
        PUT "<R33><C3><#4><FROM><R33><C80><RECT><||3>" SKIP
            "<R40><C3><FROM><R40><C80><LINE><||3>" SKIP  
            "<R33><C3><FROM><R40><C3><LINE><||3>" SKIP
            "<R33><C80><FROM><R40><C80><LINE><||3>" SKIP.
        
        PUT "<FArial><=4><R+1><C5><B>" company.name FORMAT "x(25)"  "</B>" SKIP.
        PUT "<FArial><=4><R+1><C40><B> Email: " cEmail FORMAT "x(30)"  "</B>" SKIP(2).
        PUT "<FArial><=4><R+3><C5><B>" company.addr[1] FORMAT "x(25)" "</B>" .
        PUT "<FArial><=4><R+3><C40><B> Phone: " cPhone FORMAT "x(15)" "</B>" SKIP(2).
        PUT "<FArial><=4><R+5><C5><B>" STRING(company.city,"x(15)") " " company.state FORMAT "x(2)" " " company.zip  "</B>" .
        PUT "<FArial><=4><R+5><C40><B> Fax: " cFax FORMAT "x(15)" "</B>" SKIP(2).
        
        PUT "<R44><C3><#5><FROM><R44><C80><RECT><||3>" SKIP
            "<R47><C3><FROM><R47><C80><LINE><||3>" SKIP  
            "<R44><C3><FROM><R47><C3><LINE><||3>" SKIP
            "<R44><C80><FROM><R47><C80><LINE><||3>" SKIP.
        
        PUT "<FCourier New><=5><R-2><I> Customer Name </I>" SKIP.
        PUT "<FArial><=5><R+1><B>  "  tt-word-print.cust-name FORMAT "x(30)"  "</B>" SKIP.
        
        PUT "<R50><C3><#6><FROM><R50><C40><RECT><||3>" SKIP
            "<R53><C3><FROM><R53><C40><LINE><||3>" SKIP  
            "<R50><C3><FROM><R53><C3><LINE><||3>" SKIP
            "<R50><C40><FROM><R53><C40><LINE><||3>" SKIP.
        
        PUT "<FCourier New><=6><R-2><I> Customer PO# </I>" SKIP.
        PUT "<FArial><=6><R+1><B>  " tt-word-print.cust-po-no FORMAT "x(15)"  "</B>" SKIP.
        
        PUT "<R50><C45><#7><FROM><R50><C80><RECT><||3>" SKIP
            "<R53><C45><FROM><R53><C80><LINE><||3>" SKIP  
            "<R50><C45><FROM><R53><C45><LINE><||3>" SKIP
            "<R50><C80><FROM><R53><C80><LINE><||3>" SKIP.
        
        PUT "<FCourier New><=7><R-2><I> Customer Job# </I>" SKIP.
        PUT "<FArial><=7><R+1><B>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99") "</B>" SKIP.
        
        PUT "<R56><C3><#8><FROM><R56><C80><RECT><||3>" SKIP
            "<R59><C3><FROM><R59><C80><LINE><||3>" SKIP  
            "<R56><C3><FROM><R59><C3><LINE><||3>" SKIP
            "<R56><C80><FROM><R59><C80><LINE><||3>" SKIP.
        
        PUT "<FCourier New><=8><R-2><I> Item Name </I>" SKIP.
        PUT "<FArial><=8><R+1><B>  " tt-word-print.i-name FORMAT "x(30)" "</B>" SKIP.
        
        PUT "<R62><C3><#9><FROM><R62><C40><RECT><||3>" 
            "<R65><C3><FROM><R65><C40><LINE><||3>"  
            "<R62><C3><FROM><R65><C3><LINE><||3>" 
            "<R62><C40><FROM><R65><C40><LINE><||3>" .
        
        PUT "<FCourier New><=9><R-2><I> Customer Part#</I>" SKIP.
        PUT "<FArial><=9><R+1><B>  " tt-word-print.cust-part-no FORMAT "x(30)"    "</B>" SKIP.
        
        PUT "<R62><C45><#10><FROM><R62><C80><RECT><||3>" 
            "<R65><C45><FROM><R65><C80><LINE><||3>"   
            "<R62><C45><FROM><R65><C45><LINE><||3>" 
            "<R62><C80><FROM><R65><C80><LINE><||3>" .
        
        PUT "<FCourier New><=10><R-2><I> Finished Goods Item# </I>" SKIP.
        PUT "<FArial><=10><R+1><B>  " tt-word-print.i-no FORMAT "x(15)" "</B>" SKIP.
        
        PUT "<R68><C3><#11><FROM><R68><C19><RECT><||3>"
            "<R72><C3><FROM><R72><C19><LINE><||3>"  
            "<R68><C3><FROM><R72><C3><LINE><||3>" 
            "<R68><C19><FROM><R72><C19><LINE><||3>" .
        
        PUT "<FArial><=11><R-2><B><P14> Qty / Case</B>" SKIP.
        PUT "<FArial><=11><R68.5><B><C4><P30>  " tt-word-print.pcs   "</B>" SKIP.
        
        PUT "<R68><C23><#12><FROM><R68><C38><RECT><||3>" 
            "<R72><C23><FROM><R72><C38><LINE><||3>"  
            "<R68><C23><FROM><R72><C23><LINE><||3>" 
            "<R68><C38><FROM><R72><C38><LINE><||3>" .
        
        PUT "<FArial><=12><R-2><B><P14> Cases / Pallet</B>" SKIP.
        PUT "<FArial><=12><R68.5><B><C24><P30>  "  tt-word-print.bundle "</B>" SKIP.
        
        PUT "<R68><C42><#13><FROM><R68><C59><RECT><||3>" 
            "<R72><C42><FROM><R72><C59><LINE><||3>"   
            "<R68><C42><FROM><R72><C42><LINE><||3>" 
            "<R68><C59><FROM><R72><C59><LINE><||3>" .
        PUT "<FArial><=13><R-2><B><P14> Partial Case</B>" SKIP.
        PUT "<FArial><=13><R68.5><B><C43><P30>  "  tt-word-print.partial "</B>" SKIP.
        
        PUT "<R68><C63><#14><FROM><R68><C80><RECT><||3>" 
            "<R72><C63><FROM><R72><C80><LINE><||3>"  
            "<R68><C63><FROM><R72><C63><LINE><||3>" 
            "<R68><C80><FROM><R72><C80><LINE><||3>" .
        
        PUT "<FArial><=14><R-2><B><P14> Qty Pallet</B>" SKIP.
        PUT "<FArial><=14><R68.5><B><C64><P30>  "  tt-word-print.total-unit  "</B>" SKIP.
        
        PUT "<R75><C3><#15><FROM><R75><C18><RECT><||3>" 
            "<R79><C3><FROM><R79><C18><LINE><||3>"  
            "<R75><C3><FROM><R79><C3><LINE><||3>" 
            "<R75><C18><FROM><R79><C18><LINE><||3>".
        
        PUT "<FArial><=15><R-2><B><P14> Wt / Case</B>" SKIP.
        PUT "<FArial><=15><R75.5><B><C4><P30>  " tt-word-print.case-wt "</B>" SKIP.
        
        PUT "<R75><C22><#16><FROM><R75><C48><RECT><||3>" 
            "<R79><C22><FROM><R79><C48><LINE><||3>"   
            "<R75><C22><FROM><R79><C22><LINE><||3>" 
            "<R75><C48><FROM><R79><C48><LINE><||3>" .
        
        PUT "<FCourier New><=16><R+4><C27><P14>Counted By" SKIP.
        
        PUT "<R75><C50><#17><FROM><R75><C80><RECT><||3>" 
            "<R79><C50><FROM><R79><C80><LINE><||3>"   
            "<R75><C50><FROM><R79><C50><LINE><||3>" 
            "<R75><C80><FROM><R79><C80><LINE><||3>" .
        
        PUT "<FCourier New><=17><R+4><C57>Checked By" SKIP.
        .
        
        PUT "<FCourier New>".

  END.  /* else do*/

