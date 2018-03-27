/* ------------------------------------------- ar/rep/invprot.i BPV */
/* PRINT INVOICE   Xprint form for Protagon - Header                                    */
/* -------------------------------------------------------------------------- */

lv-page = lv-page + 1.
PUT
  "<C1><R1><#1><R+15><C+40><IMAGE#1=" ls-full-img1 
  "<C1><R59><#9><R+6><C+53><IMAGE#9=" ls-full-img2 .

PUT 
  "<=1>" SKIP
  "<C1><#2>" 
  "<P10><=2><R+9>"
  "<FCourier New><B>"
  "<C6>Bill To:" 
  "<C50>Ship To:" SKIP
  "<C12>" v-soldto-name   "<C56>" v-shipto-name SKIP.

IF TRIM(v-soldto-addr[1]) NE "" THEN
   PUT "<C12>" v-soldto-addr[1].
ELSE DO:
   PUT "<C12>" v-soldto-addr[2].
   ASSIGN v-soldto-addr[2] = "".
END.

IF TRIM(v-shipto-addr[1]) NE ""
  THEN PUT "<C56>" v-shipto-addr[1] SKIP.
  ELSE DO:
   PUT "<C56>" v-shipto-addr[2] SKIP.
   ASSIGN v-shipto-addr[2] = "".
  END.

IF TRIM(v-soldto-addr[2]) NE ""
  THEN PUT "<C12>" v-soldto-addr[2].
  ELSE DO:
   PUT "<C12>" v-addr3.
   ASSIGN v-addr3 = "".
  END.

IF TRIM(v-shipto-addr[2]) NE ""
  THEN PUT "<C56>" v-shipto-addr[2] SKIP.
  ELSE DO:
   PUT "<C56>" v-sold-addr3 SKIP.
   ASSIGN v-sold-addr3 = "".
  END.

IF TRIM(v-addr3) NE ""
  THEN PUT "<C12>" v-addr3.
  
IF TRIM(v-sold-addr3) NE ""
  THEN PUT "<C56>" v-sold-addr3 SKIP.

PUT "<R16><C6>ATTENTION: " v-contact.
PUT  "</B>" SKIP.

ASSIGN v-printline = v-printline + 13.

PUT "<R2><C66>PAGE " lv-page FORMAT ">9" " of <#PAGES>".

PUT "<|10><R4><C53><#3><FROM><R8><C80.5><RECT>" SKIP.

PUT 
  "<R6><C53><FROM><R6><C80.5><LINE>" SKIP
  "<R6><C65><FROM><R7><C65><LINE>" SKIP
  "<R7><C53><FROM><R7><C80.5><LINE>" SKIP     /*Task# 11211306*/
  "<R7><C65><FROM><R8><C65><LINE>" SKIP.      /*Task# 11211306*/
        
PUT "<FArial><P12><=#3><R-2> <P10>"
    "<=#3><B><C58>Invoice # " ar-inv.inv-no "</B>"
    "<=#3><R+1><C55>Date:<C66>" formatDate(v-inv-date) FORMAT "x(20)" 
    "<=#3><R+2><C55>Customer Id:<C66>" v-custno "<FCourier New>"
    "<=#3><R+3><C57>HST#:<C66>14043 0158 RT0001"                        /*Task# 11211306*/
    SKIP(1)
    .
/*Secondary Header Boxes and Lines*/
PUT "<|10><R18><C1><#4><FROM><R21><C80.75><RECT>" SKIP
    "<R19><C1><FROM><R19><C80.75><LINE>"          SKIP    
    "<R18><C11><FROM><R21><C11><LINE>"         SKIP
    "<R18><C23><FROM><R21><C23><LINE>"         SKIP
    "<R18><C38><FROM><R21><C38><LINE>"         SKIP
    "<R18><C63><FROM><R21><C63><LINE>"         SKIP
 .

ASSIGN v-printline = v-printline + 3.

FIND FIRST sman NO-LOCK 
  WHERE sman.company EQ ar-inv.company 
    AND sman.sman    EQ v-salesman NO-ERROR.

ASSIGN v-salesname = IF AVAIL sman 
                       THEN sman.sname ELSE "".

PUT /*Secondary Header*/
 "<FArial><=4><B><C2>Ship Date"
 "<C12>FOB"
 "<C24>Ship Via"
 "<C39>Terms"
 "<C64>Salesperson" SKIP
 
  "<FCourier New><=4><R+2></B><C2>" v-date-ship FORMAT "99/99/9999" 
 "<C12>" v-fob FORMAT "x(12)" 
 "<C24>" v-shipvia FORMAT "x(16)" 
 "<C39>" ar-inv.terms-d FORMAT "x(25)" 
 "<C64>" v-salesname FORMAT "x(15)"
 "</B>"
 SKIP.

PUT 
   "<|10><B><R22><C1><#5><FROM><R59><C80.75><RECT>" SKIP /*Main Data Rectangle*/
   "<C1><R25><FROM><R25><C80.75><LINE>"             SKIP /*Main Data Header*/
       /*Main Data Column Lines*/
    "<R22><C12><FROM><R59><C12><LINE>"           SKIP /*LINE*/
    "<R22><C16><FROM><R59><C16><LINE>"           SKIP /*DESCRIPTION*/
    "<R22><C50><FROM><R59><C50><LINE>"           SKIP /*QUANTITIES*/
    "<R22><C57.5><FROM><R59><C57.5><LINE>"           SKIP /*PRICE*/
    "<R22><C67><FROM><R59><C67><LINE>"           SKIP /*AMOUNT*/
    "<R22><C77><FROM><R59><C77><LINE>"           SKIP /*TAX*/
    "</B>".   

/* Main Data Column Labels*/
PUT "<FArial><=5><B><C2>CUST PO#"
    "<C17>CUSTOMER PART #"
    "<C50.5>INVOICED"
    "<C60.5>PRICE" SKIP
    "<C2>ORDER#"
    "<C12.5>LINE"
    "<C17>DESCRIPTION"
    "<C50.5>SHIPPED"
    "<C61>UOM"
    "<C69>AMOUNT" 
    "<C77.5>TAX"   SKIP
    "<C2>BOL#"    
    "<C50.5>ORDERED"
    "</B>" SKIP(1).

PUT 
    "<FCourier New><P9>".

ASSIGN v-printline = v-printline + 3.
