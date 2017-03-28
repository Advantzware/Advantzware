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

PUT  "<FArial>".
PUT  "<C+25><#1>".
PUT  "<=1>" SKIP.
PUT  "<C1><#2>".
PUT UNFORMATTED   
       "<C4><R5.5><#1><R+90><C+80><IMAGE#1=" ls-full-img1.

 PUT "<R17><C3><P25><FROM><R17><C80><LINE>" SKIP.

 PUT "<FArial><R15><C3><p12><B>" tt-word-print.ship-add1 FORMAT "x(30)"  "</B>" .
 PUT "<FArial><=4><R15><C27><p11><B>" STRING(tt-word-print.ship-city,"x(15)") " " tt-word-print.ship-state FORMAT "x(2)" " " tt-word-print.ship-zip "</B>" .

 PUT "<FArial><=4><R15><C44><p12><B>        Phone: " cPhone FORMAT "x(15)"  "</B>" .
 PUT "<FArial><=4><R15><C64><p12><B>   Fax: " cFax FORMAT "x(15)"  "</B>" SKIP.

 PUT "<R20><C3><#4><FROM><R20><C80><RECT><||3>" SKIP
    "<R25><C3><FROM><R25><C80><LINE><||3>" SKIP  
    "<R20><C3><FROM><R25><C3><LINE><||3>" SKIP
    "<R20><C80><FROM><R25><C80><LINE><||3>" SKIP.

PUT "<FArial><=4><R18><C3><B><p15> Customer Name </B>" SKIP.
PUT "<FArial><=4><R21.2><C3><B><P24>  " tt-word-print.cust-name FORMAT "x(27)" "</B>" SKIP.

PUT "<R28><C3><#5><FROM><R28><C40><RECT><||3>" SKIP
    "<R33><C3><FROM><R33><C40><LINE><||3>" SKIP 
    "<R28><C3><FROM><R33><C3><LINE><||3>" SKIP
    "<R28><C40><FROM><R33><C40><LINE><||3>" SKIP.

PUT "<FArial><=5><R-2><B><p15> Finished Goods Item#                                  Item Name </B>" SKIP.
PUT "<FArial><=5><R29.5><C3><B><P18>  " tt-word-print.i-no FORMAT "x(15)" "</B>" SKIP.


PUT "<R28><C45><#6><FROM><R28><C80><RECT><||3>" SKIP
    "<R33><C45><FROM><R33><C80><LINE><||3>" SKIP 
    "<R28><C45><FROM><R33><C45><LINE><||3>" SKIP
    "<R28><C80><FROM><R33><C80><LINE><||3>" SKIP.

PUT "<FArial><=6><R29.5><C46><B><P17>  " tt-word-print.i-name FORMAT "x(25)"   "</B>" SKIP.

PUT "<R36><C3><#7><FROM><R36><C40><RECT><||3>" SKIP
    "<R41><C3><FROM><R41><C40><LINE><||3>" SKIP 
    "<R36><C3><FROM><R41><C3><LINE><||3>" SKIP
    "<R36><C40><FROM><R41><C40><LINE><||3>" SKIP.

PUT "<FArial><=7><R-2><B><p15> Job#                                                               Date </B>" SKIP.
PUT "<FArial><=7><R37.3><C3><B><P20>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99")  "</B>" SKIP.


PUT "<R36><C45><#8><FROM><R36><C80><RECT><||3>" SKIP
    "<R41><C45><FROM><R41><C80><LINE><||3>" SKIP 
    "<R36><C45><FROM><R41><C45><LINE><||3>" SKIP
    "<R36><C80><FROM><R41><C80><LINE><||3>" SKIP.

PUT "<FArial><=8><R37.3><C45><B><P20>  " tt-word-print.due-date FORMAT "99/99/99" "</P20></B>" SKIP.

PUT   "<#=100><AT=7.6,0.8><FROM><AT=+.8,+4><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.cust-part-no FORMAT "x(15)" ">"
    "<AT=7.3,2.1><p15>" tt-word-print.cust-part-no FORMAT "x(15)" .

PUT "<R44><C3><#9><FROM><R44><C50><RECT><||3>" SKIP
    "<R54><C3><FROM><R54><C50><LINE><||3>" SKIP 
    "<R44><C3><FROM><R54><C3><LINE><||3>" SKIP
    "<R44><C50><FROM><R54><C50><LINE><||3>" SKIP.

PUT "<FArial><=4><R42><C3><B><p15> Customer Part#  (P) </B>" SKIP.

PUT   "<#=100><AT=7.6,5.8><FROM><AT=+.8,+2><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.ord-qty ">"
    "<AT=7.3,6.1>" tt-word-print.ord-qty  .

PUT "<R44><C53><#9><FROM><R44><C80><RECT><||3>" SKIP
    "<R54><C53><FROM><R54><C80><LINE><||3>" SKIP 
    "<R44><C53><FROM><R54><C53><LINE><||3>" SKIP
    "<R44><C80><FROM><R54><C80><LINE><||3>" SKIP.
PUT "<FArial><=4><R42><C65><B><p15> Qty  (Q)</B>" SKIP.

PUT   "<#=100><AT=9.7,3.6><FROM><AT=+.8,+2><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" tt-word-print.cust-po-no FORMAT "x(15)" ">"
    "<AT=9.4,4.4>" tt-word-print.cust-po-no FORMAT "x(15)"  .

PUT "<R57><C25><#9><FROM><R57><C65><RECT><||3>" SKIP
    "<R65><C25><FROM><R65><C65><LINE><||3>" SKIP 
    "<R57><C25><FROM><R65><C25><LINE><||3>" SKIP
    "<R57><C65><FROM><R65><C65><LINE><||3>" SKIP.
PUT "<FArial><=4><R55><C36><B><p15> Customer Po#  (Z)</B>" SKIP.

 .

PUT "<FCourier New>".







