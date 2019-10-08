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
        
        PUT "<||><R11><C3><#4><FROM><R18><C80><RECT>" SKIP. 
        
        PUT "<FArial><=4><R+1><C5><B>" company.name FORMAT "x(25)"  "</B>" SKIP.
        PUT "<FArial><=4><R+1><C40><B> Email: " cEmail FORMAT "x(30)"  "</B>" SKIP(2).
        PUT "<FArial><=4><R+3><C5><B>" company.addr[1] FORMAT "x(25)" "</B>" .
        PUT "<FArial><=4><R+3><C40><B> Phone: " cPhone FORMAT "x(15)" "</B>" SKIP(2).
        PUT "<FArial><=4><R+5><C5><B>" STRING(company.city,"x(15)") " " company.state FORMAT "x(2)" " " company.zip  "</B>" .
        PUT "<FArial><=4><R+5><C40><B> Fax: " cFax FORMAT "x(15)" "</B>" SKIP(2).
        
        PUT "<||><R22><C3><#5><FROM><R25><C80><RECT>" SKIP. 
        
        PUT "<FCourier New><=5><R-2><I> Customer Name </I>" SKIP.
        PUT "<FArial><=5><R+1><B>  "  tt-word-print.cust-name FORMAT "x(30)"  "</B>" SKIP.
        
        PUT "<||><R28><C3><#6><FROM><R31><C40><RECT>" SKIP. 
        
        PUT "<FCourier New><=6><R-2><I> Customer PO# </I>" SKIP.
        PUT "<FArial><=6><R+1><B>  " tt-word-print.cust-po-no FORMAT "x(15)"  "</B>" SKIP.
        
        PUT "<||><R28><C45><#7><FROM><R31><C80><RECT>" SKIP. 
        
        PUT "<FCourier New><=7><R-2><I> Customer Job# </I>" SKIP.
        PUT "<FArial><=7><R+1><B>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99") "</B>" SKIP.
        
        PUT "<||><R34><C3><#8><FROM><R37><C80><RECT>" SKIP. 
        
        PUT "<FCourier New><=8><R-2><I> Item Name </I>" SKIP.
        PUT "<FArial><=8><R+1><B>  " tt-word-print.i-name FORMAT "x(30)" "</B>" SKIP.
        
        PUT "<||><R40><C3><#9><FROM><R43><C40><RECT>" SKIP. 
        
        PUT "<FCourier New><=9><R-2><I> Customer Part#</I>" SKIP.
        PUT "<FArial><=9><R+1><B>  " tt-word-print.cust-part-no FORMAT "x(30)"    "</B>" SKIP.
        
        PUT "<||><R40><C45><#10><FROM><R43><C80><RECT>" SKIP. 
        
        PUT "<FCourier New><=10><R-2><I> Finished Goods Item# </I>" SKIP.
        PUT "<FArial><=10><R+1><B>  " tt-word-print.i-no FORMAT "x(15)" "</B>" SKIP.
        
        PUT "<||><R46><C3><#11><FROM><R50><C19><RECT>" SKIP. 
        
        PUT "<FArial><=11><R-2><B><P14> Qty / Case</B>" SKIP.
        PUT "<FArial><=11><R46.5><B><C4><P30>  " tt-word-print.pcs   "</B>" SKIP.
        
        PUT "<||><R46><C23><#12><FROM><R50><C38><RECT>" SKIP. 
        
        PUT "<FArial><=12><R-2><B><P14> Cases / Pallet</B>" SKIP.
        PUT "<FArial><=12><R46.5><B><C24><P30>  "  tt-word-print.bundle "</B>" SKIP.
        
        PUT "<||><R46><C42><#13><FROM><R50><C59><RECT>" SKIP. 

        PUT "<FArial><=13><R-2><B><P14> Partial Case</B>" SKIP.
        PUT "<FArial><=13><R46.5><B><C43><P30>  "  tt-word-print.partial "</B>" SKIP.
        
        PUT "<||><R46><C63><#14><FROM><R50><C80><RECT>" SKIP.
            
        
        PUT "<FArial><=14><R-2><B><P14> Qty Pallet</B>" SKIP.
        PUT "<FArial><=14><R46.5><B><C64><P30>  "  tt-word-print.total-unit  "</B>" SKIP.
        
        PUT "<||5><R53><C3><#15><FROM><R57><C18><RECT>" SKIP.        
            
        
        PUT "<FArial><=15><R-2><B><P14> Wt / Case</B>" SKIP.
        PUT "<FArial><=15><R53.5><B><C4><P30>  " tt-word-print.case-wt "</B>" SKIP.
        
        PUT "<||><R53><C22><#16><FROM><R57><C48><RECT>" SKIP. 
         
        PUT "<FCourier New><=16><R+4><C27><P14>Counted By" SKIP.
        
        PUT "<||><R53><C50><#17><FROM><R57><C80><RECT>" SKIP. 
            
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
        
        PUT "<||><R33><C3><#4><FROM><R40><C80><RECT>" SKIP. 
        
        PUT "<FArial><=4><R+1><C5><B>" company.name FORMAT "x(25)"  "</B>" SKIP.
        PUT "<FArial><=4><R+1><C40><B> Email: " cEmail FORMAT "x(30)"  "</B>" SKIP(2).
        PUT "<FArial><=4><R+3><C5><B>" company.addr[1] FORMAT "x(25)" "</B>" .
        PUT "<FArial><=4><R+3><C40><B> Phone: " cPhone FORMAT "x(15)" "</B>" SKIP(2).
        PUT "<FArial><=4><R+5><C5><B>" STRING(company.city,"x(15)") " " company.state FORMAT "x(2)" " " company.zip  "</B>" .
        PUT "<FArial><=4><R+5><C40><B> Fax: " cFax FORMAT "x(15)" "</B>" SKIP(2).
        
        PUT "<||><R44><C3><#5><FROM><R47><C80><RECT>" SKIP. 
        
        PUT "<FCourier New><=5><R-2><I> Customer Name </I>" SKIP.
        PUT "<FArial><=5><R+1><B>  "  tt-word-print.cust-name FORMAT "x(30)"  "</B>" SKIP.
        
        PUT "<||><R50><C3><#6><FROM><R53><C40><RECT>" SKIP. 
        
        PUT "<FCourier New><=6><R-2><I> Customer PO# </I>" SKIP.
        PUT "<FArial><=6><R+1><B>  " tt-word-print.cust-po-no FORMAT "x(15)"  "</B>" SKIP.
        
        PUT "<||><R50><C45><#7><FROM><R53><C80><RECT>" SKIP. 
        
        PUT "<FCourier New><=7><R-2><I> Customer Job# </I>" SKIP.
        PUT "<FArial><=7><R+1><B>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99") "</B>" SKIP.
        
        PUT "<||><R56><C3><#8><FROM><R59><C80><RECT>" SKIP. 
        
        PUT "<FCourier New><=8><R-2><I> Item Name </I>" SKIP.
        PUT "<FArial><=8><R+1><B>  " tt-word-print.i-name FORMAT "x(30)" "</B>" SKIP.
        
        PUT "<||><R62><C3><#9><FROM><R65><C40><RECT>" SKIP. 
        
        PUT "<FCourier New><=9><R-2><I> Customer Part#</I>" SKIP.
        PUT "<FArial><=9><R+1><B>  " tt-word-print.cust-part-no FORMAT "x(30)"    "</B>" SKIP.
        
        PUT "<||><R62><C45><#10><FROM><R65><C80><RECT>" SKIP. 
        
        PUT "<FCourier New><=10><R-2><I> Finished Goods Item# </I>" SKIP.
        PUT "<FArial><=10><R+1><B>  " tt-word-print.i-no FORMAT "x(15)" "</B>" SKIP.
        
        PUT "<||><R68><C3><#11><FROM><R72><C19><RECT>" SKIP. 
        
        PUT "<FArial><=11><R-2><B><P14> Qty / Case</B>" SKIP.
        PUT "<FArial><=11><R68.5><B><C4><P30>  " tt-word-print.pcs   "</B>" SKIP.
        
        PUT "<||><R68><C23><#12><FROM><R72><C38><RECT>" SKIP. 
        
        PUT "<FArial><=12><R-2><B><P14> Cases / Pallet</B>" SKIP.
        PUT "<FArial><=12><R68.5><B><C24><P30>  "  tt-word-print.bundle "</B>" SKIP.
        
        PUT "<||><R68><C42><#13><FROM><R72><C59><RECT>" SKIP. 

        PUT "<FArial><=13><R-2><B><P14> Partial Case</B>" SKIP.
        PUT "<FArial><=13><R68.5><B><C43><P30>  "  tt-word-print.partial "</B>" SKIP.
        
        PUT "<||><R68><C63><#14><FROM><R72><C80><RECT>" SKIP. 
        
        PUT "<FArial><=14><R-2><B><P14> Qty Pallet</B>" SKIP.
        PUT "<FArial><=14><R68.5><B><C64><P30>  "  tt-word-print.total-unit  "</B>" SKIP.
        
        PUT "<||><R75><C3><#15><FROM><R79><C18><RECT>" SKIP. 
        
        PUT "<FArial><=15><R-2><B><P14> Wt / Case</B>" SKIP.
        PUT "<FArial><=15><R75.5><B><C4><P30>  " tt-word-print.case-wt "</B>" SKIP.
        
        PUT "<||><R75><C22><#16><FROM><R79><C48><RECT>" SKIP. 
        
        PUT "<FCourier New><=16><R+4><C27><P14>Counted By" SKIP.
        
        PUT "<||><R75><C50><#17><FROM><R79><C80><RECT>" SKIP. 
        
        PUT "<FCourier New><=17><R+4><C57>Checked By" SKIP.
        .
        
        PUT "<FCourier New>".

  END.  /* else do*/

