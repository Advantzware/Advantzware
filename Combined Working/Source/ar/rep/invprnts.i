/* ------------------------------------------- ar/rep/invprnts.i BV */
/* PRINT INVOICE   Xprint form for Printers (Loylang) - copied from Soule                                    */
/* -------------------------------------------------------------------------- */
/*****************************************************************************
  PUT "<FTimes New Roman>".
  PUT "<C+20><#1><B><P20>" skip
      space(24) v-comp-add1 FORM "x(50)"  "</B>" SKIP(1)
     "<P10>" space(65) v-comp-add2 FORM "x(60)" SKIP
     space(75) v-comp-add3 FORM "x(60)" SKIP
     space(62) v-comp-add4 FORM "x(60)" "<P10>"SKIP.
*****************************************************************************/
PUT
/*   "<C7><#1><R+11><C+25><IMAGE#1=" ls-full-img1 SKIP. */
  "<C2><R3><#1><R+60><C+50><IMAGE#1=" ls-full-img1 .
PUT 
  "<=1>" SKIP
  "<C1><#2>"
  "<P10><=2><R+11>"
  "<FCourier New><B>"
  SPACE(5) "Sold To:" SPACE(45) "Ship To:" SKIP
  SPACE(12) ar-inv.cust-name v-shipto-name AT 65 SKIP.

IF TRIM(ar-inv.addr[1]) NE ""
  THEN PUT SPACE(12) ar-inv.addr[1].
  ELSE DO:
   PUT SPACE(12) ar-inv.addr[2].
   ASSIGN v-addr2 = "".
  END.

IF TRIM(v-shipto-addr[1]) NE ""
  THEN PUT v-shipto-addr[1] AT 65 SKIP.
  ELSE DO:
   PUT v-shipto-addr[2] AT 65 SKIP.
   ASSIGN v-shipto-addr[2] = "".
  END.

IF TRIM(ar-inv.addr[2]) NE ""
  THEN PUT SPACE(12) ar-inv.addr[2].
  ELSE DO:
   PUT SPACE(12) v-addr3.
   ASSIGN v-addr3 = "".
  END.

IF TRIM(v-shipto-addr[2]) NE ""
  THEN PUT v-shipto-addr[2] AT 65 SKIP.
  ELSE DO:
   PUT v-sold-addr3 AT 65 SKIP.
   ASSIGN v-sold-addr3 = "".
  END.

IF TRIM(v-addr3) NE ""
  THEN PUT SPACE(12) v-addr3.
  
IF TRIM(v-sold-addr3) NE ""
  THEN PUT v-sold-addr3 AT 65 SKIP.
  
PUT  "</B>" SKIP.

ASSIGN v-printline = v-printline + 15.

PUT "<|5><R4><C53><#3><FROM><R11><C78><RECT>" SKIP.

PUT 
  "<R7><C53><FROM><R7><C78><LINE>" SKIP
  "<R8><C53><FROM><R8><C78><LINE>" SKIP
  "<R9><C53><FROM><R9><C78><LINE>" SKIP
  "<R10><C53><FROM><R10><C78><LINE>" SKIP
  "<R7><C65><FROM><R11><C65><LINE>" SKIP.  
        
PUT "<FArial><P10><=#5><R-2> <P10>" SKIP
    "<=#3><B>                         INVOICE                  " SKIP
    "<=#3><R+1><P14>                 " ar-inv.inv-no "</B><P10>"
    "<=#3><R+3>                      Date    " v-inv-date SKIP 
    "<=#3><R+4>         Customer Id     " v-custno SKIP
    "<=#3><R+5>    Purchase Order     " v-po-no SKIP 
    "<=#3><R+6>          Job Number    " v-job-no "<FCourier New>"
    SKIP(1)
    .

PUT "<|5><R21><C1><#4><FROM><R24><C81><RECT>" SKIP
    "<R22><C1><FROM><R22><C81><LINE>"          SKIP
    "<R21><C11><FROM><R24><C11><LINE>"         SKIP
    "<R21><C22><FROM><R24><C22><LINE>"         SKIP
    "<R21><C38><FROM><R24><C38><LINE>"         SKIP
    "<R21><C52><FROM><R24><C52><LINE>"         SKIP
    "<R21><C72><FROM><R24><C72><LINE>"         SKIP
    .

ASSIGN v-printline = v-printline + 3.

FIND FIRST sman NO-LOCK
  WHERE sman.company EQ ar-inv.company 
    AND sman.sman    EQ v-salesman NO-ERROR.

ASSIGN v-salesname = IF AVAIL sman 
                       THEN sman.sname ELSE "".

PUT 
  "<FArial><=4><B>  Ship Date             FOB                    Ship Via                                 Terms                               Sales Person                   BOL#" SKIP
  "<FCourier New><=4><R+2></B> " v-date-ship FORMAT "99/99/9999" 
 SPACE(2)
  v-fob          FORMAT "x(12)" 
 SPACE(1)
  v-shipvia      FORMAT "x(20)" 
 SPACE(1)
  ar-inv.terms-d FORMAT "x(15)" 
 SPACE(5) 
  v-salesname    FORMAT "x(15)"
 SPACE(3) 
 lv-bol-no  "</B>"
 SKIP.

PUT 
   "<|5><B><R25><C1><#5><FROM><R63><C81><RECT>" SKIP 
   "<C1><R27><FROM><R27><C81><LINE>"             SKIP 
   "<C59><R59><FROM><R59><C81><LINE>"            SKIP

    "<R25><C15><FROM><R60><C15><LINE>"           SKIP 
    "<R25><C41><FROM><R60><C41><LINE>"           SKIP 
    "<R27><C48.5><FROM><R60><C48.5><LINE>"       SKIP 
    "<R25><C56><FROM><R60><C56><LINE>"           SKIP 
    "<R25><C59><FROM><R61><C59><LINE>"           SKIP 
    "<R25><C70><FROM><R59><C70><LINE>"           SKIP 
    "</B>" .   

PUT 
  "<R60><C11><P8></B> CLAIMS MUST BE MADE ON RECEIPT OF SHIPMENTS OTHERWISE NOT ALLOWED." .

PUT 
  "<FArial><R61><C1><#9><FROM><R63><C59><RECT> " 
  "<R61><C33><FROM><R63><C33><LINE>"         
  "<R61><C59><FROM><R63><C59><LINE>" SKIP .

PUT
  "<=9><R61><C1.5><P6>THIS IS YOUR INVOICE - MONTHLY STATEMENTS NOT RENDERED. IF    "
  "<C34>1 1/2% PENALTY PER MONTH WILL BE CHARGED ($1.00"         
 SKIP
  "<=9><R61.5><C1.5>LEGAL ACTION IS REQUIRED FOR COLLECTION OF THIS INVOICE, THE  "
  "<C34>MINIMUM)   TO   YOUR  ACCOUNT ON BALANCES OVER" 
 SKIP
  "<=9><R62><C1.5>BUYER IS RESPONSIBLE FOR ALL LEGAL EXPENSES INCURRED."
  "<C34>30 DAYS."     
  .

PUT
  "<R63><P10><B>ORIGINAL INVOICE</B>" 
  " " SKIP
  SPACE (60) "<B>THANK YOU, YOUR BUSINESS IS GREATLY APPRECIATED!</B>".

PUT "<FArial><=5><B><C45>QUANTITY" SKIP
    "<C1> CUSTOMER PART#"
    "<C15> FINISHED GOOD#/DESCRIPTION"
    "<C41> ORDERED  INVOICED"
    "<C56> P/C"
    "<C61>PRICE/UOM"
    "<C73>AMOUNT" SKIP
    "</B>"SKIP.

PUT "<FCourier New>".

ASSIGN v-printline = v-printline + 2.
           

