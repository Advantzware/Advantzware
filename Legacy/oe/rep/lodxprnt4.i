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

 PUT "<R20><C3><#4><FROM><R20><C80><RECT><||3>" SKIP
    "<R25><C3><FROM><R25><C80><LINE><||3>" SKIP  
    "<R20><C3><FROM><R25><C3><LINE><||3>" SKIP
    "<R20><C80><FROM><R25><C80><LINE><||3>" SKIP.

PUT "<FArial><=4><R18><C3><B><p15> Customer Name </B>" SKIP.
PUT "<FArial><=4><R21.2><C3><B><P24>  " tt-word-print.cust-name FORMAT "x(27)" "</B>" SKIP.

PUT "<R28><C3><#5><FROM><R28><C80><RECT><||3>" SKIP
    "<R33><C3><FROM><R33><C80><LINE><||3>" SKIP 
    "<R28><C3><FROM><R30><C3><LINE><||3>" SKIP
    "<R31><C3><FROM><R33><C3><LINE><||3>" SKIP
    "<R28><C80><FROM><R33><C80><LINE><||3>" SKIP.

PUT "<FArial><=5><R-2><B><p15> PO#                                                                Customer Part# </B>" SKIP.
PUT "<FArial><=5><R29><C3><B><P23>  " tt-word-print.cust-po-no FORMAT "x(15)"  "</B>" SKIP.
PUT "<FArial><=5><R29><C43><B><P26>  " tt-word-print.cust-part-no FORMAT "x(15)" "</B>" SKIP.

PUT "<R36><C3><#5><FROM><R36><C80><RECT><||3>" SKIP
    "<R41><C3><FROM><R41><C80><LINE><||3>" SKIP 
    "<R36><C3><FROM><R39><C3><LINE><||3>" SKIP
    "<R40><C3><FROM><R41><C3><LINE><||3>" SKIP
    "<R36><C80><FROM><R41><C80><LINE><||3>" SKIP.

PUT "<FArial><=5><R-2><B><p15> Item Name</B>" SKIP.
PUT "<FArial><=5><R37><C3><B><P26>  " tt-word-print.i-name FORMAT "x(30)" "</B>" SKIP.

PUT "<R44><C3><#5><FROM><R44><C80><RECT><||3>" SKIP
    "<R50><C3><FROM><R50><C80><LINE><||3>" SKIP  
    "<R44><C3><FROM><R50><C3><LINE><||3>" SKIP
    "<R44><C80><FROM><R50><C80><LINE><||3>" SKIP.

PUT "<FArial><=5><R-2><B><p15> Finished Goods Item#</B>" SKIP.
PUT "<FArial><=5><R45.5><C3><B><P26>  " tt-word-print.i-no FORMAT "x(15)" "</B>" SKIP.


PUT "<R55><C3><#10><FROM><R55><C18><RECT><||3>" SKIP
    "<R60><C3><FROM><R60><C18><LINE><||3>" SKIP  
    "<R55><C3><FROM><R60><C3><LINE><||3>" SKIP
    "<R55><C18><FROM><R60><C18><LINE><||3>" SKIP.

PUT "<FArial><=10><R53><C4><B><P22> Qty </B>" SKIP.
PUT "<FArial><=10><R56><C2><B><P21>" tt-word-print.ord-qty  "</B>" SKIP.

PUT "<R55><C20><#11><FROM><R55><C29><RECT><||3>" SKIP
    "<R60><C20><FROM><R60><C29><LINE><||3>" SKIP  
    "<R55><C20><FROM><R60><C20><LINE><||3>" SKIP
    "<R55><C29><FROM><R60><C29><LINE><||3>" SKIP.

PUT "<FArial><=11><R53><C24><B><P26> PALLET </B>" SKIP.
PUT "<FArial><=11><R56><C19><B><P24>  " tt-word-print.pcs "</B>" SKIP.
PUT "<FArial><=11><R56><C29><B><P26> of </B>" .

PUT "<R55><C35><#12><FROM><R55><C43><RECT><||3>" SKIP
    "<R60><C35><FROM><R60><C43><LINE><||3>" SKIP  
    "<R55><C35><FROM><R60><C35><LINE><||3>" SKIP
    "<R55><C43><FROM><R60><C43><LINE><||3>" SKIP.
PUT "<FArial><=12><R56><C33.8><B><P24>  " tt-word-print.pcs "</B>" SKIP.


PUT "<R55><C45><#13><FROM><R55><C62><RECT><||3>" SKIP
    "<R60><C45><FROM><R60><C62><LINE><||3>" SKIP  
    "<R55><C45><FROM><R60><C45><LINE><||3>" SKIP
    "<R55><C62><FROM><R60><C62><LINE><||3>" SKIP.

PUT "<FArial><=10><R53><C47><B><P22> Job# </B>" SKIP.
PUT "<FArial><=10><R56><C44><B><P24>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99") "</B>" SKIP.

PUT "<R55><C64><#14><FROM><R55><C80><RECT><||3>" SKIP
    "<R60><C64><FROM><R60><C80><LINE><||3>" SKIP  
    "<R55><C64><FROM><R60><C64><LINE><||3>" SKIP
    "<R55><C80><FROM><R60><C80><LINE><||3>" SKIP.

PUT "<FArial><=10><R53><C65><B><P22> Date </B>" SKIP.
PUT "<FArial><=10><R56><C64><B><P24>  " tt-word-print.due-date FORMAT "99/99/99"  "</B>" SKIP.


 .

PUT "<FCourier New>".







