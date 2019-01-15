/* oe/rep/invxprnt.i */

 FIND FIRST cust NO-LOCK
     WHERE cust.company EQ cocode
       AND cust.active = "X" NO-ERROR .
 IF AVAIL cust THEN DO:
     ASSIGN
       cEmail = cust.email .
       cPhone = string(cust.area-code,"(xxx)") +  string(cust.phone,"xxx-xxxx") .
       cFax   = STRING(SUBSTR(cust.fax,1,3),"(xxx)") + "-" + STRING(SUBSTR(cust.fax,4),"xxx-xxxx")   .
 END.
     FIND FIRST company NO-LOCK
          WHERE company.company  = cocode  NO-ERROR.
    IF iplPrintView THEN do: 
        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        PUT   "<#=100><AT=,2><FROM><AT=+.8,+4><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.tag-no FORMAT "x(20)"  ">"
            "<AT=,3>" tt-word-print.tag-no FORMAT "x(20)"  .
        
        PUT  SKIP(3) "<R9><C3><FROM><R9><C80><LINE>" SKIP .
        
        PUT "<||><R10><C3><#4><FROM><R17><C80><RECT>" SKIP.    
    
        PUT "<FArial><=4><R11><C5><P17><B>" company.NAME FORMAT "x(30)"  "</B>" SKIP.
        PUT "<FArial><=4><R11><C44><P17><B>" company.NAME FORMAT "x(30)"  "</B>" SKIP.
        PUT "<FArial><=4><R13><C5><P15><B>" company.addr[1] FORMAT "x(30)" "</B>" .
        PUT "<FArial><=4><R13><C44><P15><B> Phone: " cPhone FORMAT "x(15)"  "</B>" SKIP(2).
        PUT "<FArial><=4><R15><C5><P15><B>" company.city " " company.state FORMAT "x(2)" " " company.zip "</B>" .
        PUT "<FArial><=4><R15><C44><P15><B> Fax: " cFax FORMAT "x(15)" "</B>" SKIP(2).
        
        PUT "<||><R20><C3><#5><FROM><R25><C80><RECT>" SKIP.
        
        PUT "<FArial><=5><R-2><B><p14> Customer Name </B>" SKIP.
        PUT "<FArial><=5><R20.9><B><P27>  " tt-word-print.cust-name FORMAT "x(30)"   "</B>" SKIP.
        
        PUT "<||><R28><C3><#6><FROM><R32><C80><RECT>" SKIP.
        
        PUT "<FArial><=6><R-2><B><p14> Customer PO# </B>" SKIP.
        PUT "<FArial><=6><R29><C4><B><P24>  " tt-word-print.cust-po-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<||><R35><C3><#7><FROM><R39><C80><RECT>" SKIP.
        
        PUT "<FArial><=7><R-2><B><P14> Customer Part# </B>" SKIP.
        PUT "<FArial><=7><R35.5><C2><B><P32>  " tt-word-print.cust-part-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<||><R42><C3><#8><FROM><R48><C80><RECT>" SKIP.
        
        PUT "<FArial><=8><R-2><B><P14> Item Description </B>" SKIP.
        PUT "<FArial><=8><R43.5><C2><B><P22>  " tt-word-print.i-name FORMAT "x(30)"  "</B>" SKIP.
        
        PUT "<||><R51><C3><#9><FROM><R55><C80><RECT>" SKIP.
        
        PUT "<FArial><=9><R-2><B><P14> Finished Goods Item# </B>" SKIP.
        PUT "<FArial><=9><R51.5><C2><B><P30>  " tt-word-print.i-no FORMAT "x(15)"   "</B>" SKIP.        
        
        PUT "<||><R58><C3><#10><FROM><R63><C18><RECT>" SKIP.
        
        PUT "<FArial><=10><R56><C4><B><P22> Qty/Pallet </B>" SKIP.
        PUT "<FArial><=10><R59><C2><B><P24>  " tt-word-print.total-unit FORMAT ">,>>>,>>9" "</B>" SKIP.
        
        PUT "<||><R58><C20><#11><FROM><R63><C29><RECT>" SKIP.
        
        PUT "<FArial><=11><R56><C24><B><P26> PALLET </B>" SKIP.
        PUT "<FArial><=11><R59><C19><B><P24>  " tt-word-print.pcs "</B>" SKIP.
        
        PUT "<FArial><=11><R59><C29><B><P26> of </B>" .

        PUT "<||><R58><C35><#12><FROM><R63><C43><RECT>" SKIP.

        PUT "<FArial><=12><R59><C33.8><B><P24>  " tt-word-print.pcs "</B>" SKIP.       
        
        PUT "<||><R58><C45><#13><FROM><R63><C62><RECT>" SKIP.
        
        PUT "<FArial><=10><R56><C47><B><P22> Job# </B>" SKIP.
        PUT "<FArial><=10><R59><C44><B><P24>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99")  "</B>" SKIP.
        
        PUT "<||><R58><C64><#14><FROM><R63><C80><RECT>" SKIP.
        
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
        PUT   "<#=100><AT=3.30,2><FROM><AT=+.8,+4><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.tag-no FORMAT "x(20)"  ">"
            "<AT=4.25,3>" tt-word-print.tag-no FORMAT "x(20)"  .
        
        PUT  SKIP(3) "<R29><C3><FROM><R29><C80><LINE>" SKIP .       
        
        PUT "<||><R30><C3><#4><FROM><R37><C80><RECT>" SKIP.
        
        PUT "<FArial><=4><R31><C5><P17><B>" company.NAME FORMAT "x(30)"  "</B>" SKIP.
        PUT "<FArial><=4><R31><C44><P17><B>" company.NAME FORMAT "x(30)"  "</B>" SKIP.
        PUT "<FArial><=4><R33><C5><P15><B>" company.addr[1] FORMAT "x(30)" "</B>" .
        PUT "<FArial><=4><R33><C44><P15><B> Phone: " cPhone FORMAT "x(15)"  "</B>" SKIP(2).
        PUT "<FArial><=4><R35><C5><P15><B>" company.city " " company.state FORMAT "x(2)" " " company.zip "</B>" .
        PUT "<FArial><=4><R35><C44><P15><B> Fax: " cFax FORMAT "x(15)" "</B>" SKIP(2).
        
        PUT "<||><R40><C3><#5><FROM><R45><C80><RECT>" SKIP.
        
        PUT "<FArial><=5><R-2><B><p14> Customer Name </B>" SKIP.
        PUT "<FArial><=5><R40.9><B><P27>  " tt-word-print.cust-name FORMAT "x(30)"   "</B>" SKIP.
        
        PUT "<||><R48><C3><#6><FROM><R52><C80><RECT>" SKIP.
        
        PUT "<FArial><=6><R-2><B><p14> Customer PO# </B>" SKIP.
        PUT "<FArial><=6><R49><C4><B><P24>  " tt-word-print.cust-po-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<||><R55><C3><#7><FROM><R59><C80><RECT>" SKIP.
        
        PUT "<FArial><=7><R-2><B><P14> Customer Part# </B>" SKIP.
        PUT "<FArial><=7><R55.5><C2><B><P32>  " tt-word-print.cust-part-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<||><R62><C3><#8><FROM><R68><C80><RECT>" SKIP.
        
        PUT "<FArial><=8><R-2><B><P14> Item Description </B>" SKIP.
        PUT "<FArial><=8><R63.5><C2><B><P22>  " tt-word-print.i-name FORMAT "x(30)"  "</B>" SKIP.
        
        PUT "<||><R71><C3><#9><FROM><R75><C80><RECT>" SKIP.
        
        PUT "<FArial><=9><R-2><B><P14> Finished Goods Item# </B>" SKIP.
        PUT "<FArial><=9><R71.5><C2><B><P30>  " tt-word-print.i-no FORMAT "x(15)"   "</B>" SKIP.
        
        PUT "<||><R78><C3><#10><FROM><R83><C18><RECT>" SKIP.
        
        PUT "<FArial><=10><R76><C4><B><P22> Qty/Pallet </B>" SKIP.
        PUT "<FArial><=10><R79><C2><B><P24>  " tt-word-print.total-unit FORMAT ">,>>>,>>9" "</B>" SKIP.
        
        PUT "<||><R78><C20><#11><FROM><R83><C29><RECT>" SKIP.
        
        PUT "<FArial><=11><R76><C24><B><P26> PALLET </B>" SKIP.
        PUT "<FArial><=11><R79><C19><B><P24>  " tt-word-print.pcs "</B>" SKIP.
        
        PUT "<FArial><=11><R79><C29><B><P26> of </B>" .
       
        PUT "<||><R78><C35><#12><FROM><R83><C43><RECT>" SKIP.

        PUT "<FArial><=12><R79><C33.8><B><P24>  " tt-word-print.pcs "</B>" SKIP.
        
        PUT "<||><R78><C45><#13><FROM><R83><C62><RECT>" SKIP.
        
        PUT "<FArial><=10><R76><C47><B><P22> Job# </B>" SKIP.
        PUT "<FArial><=10><R79><C44><B><P24>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99")  "</B>" SKIP.
        
        PUT "<||><R78><C64><#14><FROM><R83><C80><RECT>" SKIP.
        
        PUT "<FArial><=10><R76><C65><B><P22> Date </B>" SKIP.
        PUT "<FArial><=10><R79><C64><B><P24>  " tt-word-print.due-date FORMAT "99/99/99"  "</B><P12>" SKIP.
        .
        
        PUT "<FCourier New>".
    END.







