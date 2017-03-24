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
PUT "<FArial><=4><R7><C14><P26><B>" company.NAME FORMAT "x(25)" "</B>" SKIP(2).
PUT "<FArial><=4><R10><C28><P22><B>" tt-word-print.ship-add1 FORMAT "x(30)" "</B>" .
PUT "<FArial><=4><R12><C28><P22><B>" STRING(tt-word-print.ship-city,"x(15)") " " tt-word-print.ship-state FORMAT "x(2)" " " tt-word-print.ship-zip "</B>" .

PUT  SKIP(3) "<R17><C3><FROM><R17><C80><LINE>" SKIP .

PUT "<R20><C3><#5><FROM><R20><C80><RECT><||3>" SKIP
    "<R25><C3><FROM><R25><C80><LINE><||3>" SKIP  
    "<R20><C3><FROM><R25><C3><LINE><||3>" SKIP
    "<R20><C80><FROM><R25><C80><LINE><||3>" SKIP.

PUT "<FArial><=5><R-2><B><p14> Customer Name </B>" SKIP.
PUT "<FArial><=5><R+1><B><P27>  " tt-word-print.cust-name FORMAT "x(30)" "</B>" SKIP.

PUT "<R28><C3><#6><FROM><R28><C40><RECT><||3>" SKIP
    "<R32><C3><FROM><R32><C40><LINE><||3>" SKIP  
    "<R28><C3><FROM><R29><C3><LINE><||3>" SKIP
    "<R30><C3><FROM><R32><C3><LINE><||3>" SKIP
    "<R28><C40><FROM><R32><C40><LINE><||3>" SKIP.

PUT "<FArial><=6><R-2><B><p14> PO# </B>" SKIP.
PUT "<FArial><=6><R29><C4><B><P24>  " tt-word-print.po-no FORMAT ">>>>>>>>>" "</B>" SKIP.

PUT "<R28><C45><#6><FROM><R28><C80><RECT><||3>" SKIP
    "<R32><C45><FROM><R32><C80><LINE><||3>" SKIP 
    "<R28><C45><FROM><R32><C45><LINE><||3>" SKIP
    "<R28><C80><FROM><R32><C80><LINE><||3>" SKIP.

PUT "<FArial><=6><R-2><B><p14> Customer PO# </B>" SKIP.
PUT "<FArial><=6><R29><C46><B><P24>  " tt-word-print.cust-po-no FORMAT "x(15)" "</B>" SKIP.


PUT "<R35><C3><#7><FROM><R35><C80><RECT><||3>" SKIP
    "<R39><C3><FROM><R39><C80><LINE><||3>" SKIP  
    "<R35><C3><FROM><R39><C3><LINE><||3>" SKIP
    "<R35><C80><FROM><R39><C80><LINE><||3>" SKIP.

PUT "<FArial><=7><R-2><B><P14> Item Name </B>" SKIP.
PUT "<FArial><=7><R35.5><C2><B><P30>  " tt-word-print.i-name FORMAT "x(30)" "</B>" SKIP.

PUT "<R42><C3><#8><FROM><R42><C80><RECT><||3>" SKIP
    "<R48><C3><FROM><R48><C80><LINE><||3>" SKIP  
    "<R42><C3><FROM><R48><C3><LINE><||3>" SKIP
    "<R42><C80><FROM><R48><C80><LINE><||3>" SKIP.

PUT "<FArial><=8><R-2><B><P14> Customer Part# </B>" SKIP.
PUT "<FArial><=8><R43.5><C2><B><P30>  " tt-word-print.cust-part-no FORMAT "x(15)" "</B>" SKIP.

PUT "<R51><C3><#9><FROM><R51><C80><RECT><||3>" SKIP
    "<R55><C3><FROM><R55><C80><LINE><||3>" SKIP  
    "<R51><C3><FROM><R55><C3><LINE><||3>" SKIP
    "<R51><C80><FROM><R55><C80><LINE><||3>" SKIP.

PUT "<FArial><=9><R-2><B><P14> Finished Goods Item# </B>" SKIP.
PUT "<FArial><=9><R51.5><C2><B><P30>  " tt-word-print.i-no FORMAT "x(15)" "</B>" SKIP.

PUT "<R58><C3><#10><FROM><R58><C18><RECT><||3>" SKIP
    "<R63><C3><FROM><R63><C18><LINE><||3>" SKIP  
    "<R58><C3><FROM><R63><C3><LINE><||3>" SKIP
    "<R58><C18><FROM><R63><C18><LINE><||3>" SKIP.

PUT "<FArial><=10><R56><C4><B><P22> Qty </B>" SKIP.
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







