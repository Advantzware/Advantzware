/* ------------------------------------------- ar/rep/invloyjit.i */
/* PRINT INVOICE   Xprint form for Loylangjit                                 */
/* -------------------------------------------------------------------------- */

PUT
  "<C2><R3><#1><R+60><C+50><IMAGE#1=" ls-full-img1
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

ASSIGN v-printline = v-printline + 15.

PUT "<|10><R4><C53><#3><FROM><R9><C78><RECT>" SKIP.

PUT 
  "<R7><C53><FROM><R7><C78><LINE>" SKIP
  "<R8><C53><FROM><R8><C78><LINE>" SKIP
  "<R7><C65><FROM><R9><C65><LINE>" SKIP.  
        
PUT "<FArial><P12><=#3><R-2> <P10>" SKIP
    "<=#3><B>                       INVOICE#                    " SKIP
    "<=#3><R+1><P14>                 "ar-inv.inv-no "</B><P10>"
    "<=#3><R+3>              DATE               " v-inv-date SKIP
    "<=#3><R+4>       Customer Id:          " v-custno "<FCourier New>"    
    SKIP(1)
    .

PUT "<|10><R21><C1><#4><FROM><R24><C80.75><RECT>" SKIP
    "<R22><C1><FROM><R22><C80.75><LINE>"          SKIP
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
  "<FArial><=4><B>  Ship Date             FOB                    Ship Via                                 Terms                               Sales Person                       BOL#" SKIP
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
   "<|10><B><R25><C1><#5><FROM><R63><C80.75><RECT>" SKIP 
   "<C1><R28><FROM><R28><C80.75><LINE>"             SKIP 
   "<C61><R59><FROM><R59><C80.75><LINE>"            SKIP

    "<R25><C15><FROM><R60><C15><LINE>"           SKIP 
    "<R25><C41><FROM><R60><C41><LINE>"           SKIP 
    "<R28><C48.5><FROM><R60><C48.5><LINE>"       SKIP 
    "<R25><C57><FROM><R60><C57><LINE>"           SKIP 
    "<R25><C61><FROM><R61><C61><LINE>"           SKIP 
    "<R25><C72><FROM><R59><C72><LINE>"           SKIP 
    "</B>".   

PUT 
  "<R60><C13><P8></B> CLAIMS MUST BE MADE ON RECEIPT OF SHIPMENTS OTHERWISE NOT ALLOWED." .

PUT 
  "<FArial><R61><C1><#9><FROM><R63><C61><RECT> " 
  "<R61><C34.5><FROM><R63><C34.5><LINE>"         
  "<R61><C61><FROM><R63><C61><LINE>" SKIP .

PUT
  "<=9><R61><P6> THIS IS YOUR INVOICE - MONTHLY STATEMENTS NOT RENDERED. IF"
  "                         1 1/2% PENALTY PER MONTH WILL BE CHARGED ($1.00"         
 SKIP
  "<=9><R61.5> LEGAL ACTION IS REQUIRED FOR COLLECTION OF THIS INVOICE. THE"
  "                       MINIMUM)   TO   YOUR  ACCOUNT ON BALANCES OVER" 
 SKIP
  "<=9><R62> BUYER IS RESPONSIBLE FOR ALL LEGAL EXPENSES INCURRED."
  "                                  30 DAYS."                
  .

PUT
  "<R63><P10><B>ORIGINAL INVOICE</B>" 
  " " SKIP  
 "<BGCOLOR=255,255,0><R64><C19><FROM><R65><C70><FILLRECT>"
"<R64><C20><P11><B>Remit Payment to: 222 Russell Boulevard, St. Louis, MO 63104-4608</B><P10>". 

PUT "<FArial><=5><B>   CUST PO#                     CUSTOMER PART #                                     QUANTITY                                  PRICE" SKIP
    "   ORDER#                           DESCRIPTION                                     SHIPPED      INVOICED   P/C            UOM             AMOUNT"    SKIP
    " CUSTOMER LOTS #                                                                      ORDERED"
    "</B><FCourier New>" SKIP(1).

ASSIGN v-printline = v-printline + 3.
           
PUT "</B>".
