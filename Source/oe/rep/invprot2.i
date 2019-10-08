/* ------------------------------------------- oe/rep/invprot2.i BPV */
/* PRINT INVOICE   Xprint form for Protagon2 - Header                                    */
/* -------------------------------------------------------------------------- */

lv-page = lv-page + 1.
PUT
  "<C1><R1><#1><R+15><C+40><IMAGE#1=" ls-full-img1 .
PUT 
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


PUT "<R2><C66>PAGE " lv-page FORMAT ">9" " of <#PAGES>".

PUT "<|10><R4><C53><#3><FROM><R8><C80.5><RECT>" SKIP.

PUT 
  "<R6><C53><FROM><R6><C80.5><LINE>" SKIP
  "<R6><C65><FROM><R7><C65><LINE>" SKIP
  "<R7><C53><FROM><R7><C80.5><LINE>" SKIP     /*Task# 11211306*/
  "<R7><C65><FROM><R8><C65><LINE>" SKIP.      /*Task# 11211306*/

PUT "<FArial><P12><=#3><R-2> <P10>"
    "<=#3><B><C58>Invoice # " xinv-head.inv-no "</B>"
    "<=#3><R+1><C55>Date:<C66>" formatDate(v-inv-date) FORMAT "x(20)" 
    "<=#3><R+2><C55>Customer Id:<C66>" v-custno "<FCourier New>" 
    "<=#3><R+3><C57>HST#:<C66>14043 0158 RT0001"                        /*Task# 11211306*/
    SKIP(1)
    .

/*Secondary Header Boxes and Lines*/
PUT "<|10><R18><C1><#4><FROM><R21><C80.75><RECT>" SKIP
    "<R19><C1><FROM><R19><C80.75><LINE>"          SKIP    
    "<R18><C14><FROM><R21><C14><LINE>"         SKIP
    "<R18><C38><FROM><R21><C38><LINE>"         SKIP
    "<R18><C63><FROM><R21><C63><LINE>"         SKIP
 .

FIND FIRST sman NO-LOCK 
  WHERE sman.company EQ inv-head.company 
    AND sman.sman    EQ v-salesman NO-ERROR.

ASSIGN v-salesname = IF AVAIL sman 
                       THEN sman.sname ELSE "".

PUT /*Secondary Header*/
 "<FArial><=4><B><C2>PO Number"
 "<C15>BOL #s"
 "<C39>Terms"
 "<C64>Salesperson</B>" SKIP

 "<FCourier New><=4>"
 "<R+1><C2>" v-po-no FORMAT "X(15)"
 "<C15>" lv-bol-list-1 FORMAT "X(27)"
 "<C39>" lv-terms FORMAT "x(25)" 
 "<C64>" v-salesname FORMAT "x(15)"
 "<R+1><C15>" lv-bol-list-2 FORMAT "X(27)"
 SKIP.


PUT 
   "<|10><B><R22><C1><#5><FROM><R59><C80.75><RECT>" SKIP /*Main Data Rectangle*/
   "<C1><R25><FROM><R25><C80.75><LINE>"             SKIP /*Main Data Header*/
       /*Main Data Column Lines*/
    "<R22><C9><FROM><R59><C9><LINE>"           SKIP /*DESCRIPTION*/
    "<R22><C54><FROM><R59><C54><LINE>"           SKIP /*PRICE*/
    "<R22><C64><FROM><R59><C64><LINE>"           SKIP /*AMOUNT*/
    "<R22><C74><FROM><R59><C74><LINE>"           SKIP /*TAX*/
    "</B>".   

/* Main Data Column Labels*/
PUT "<FArial><=5><B><R+0.5><C1.5>QUANTITY"
    "<C57>PRICE" SKIP
    "<=5><R+1><C26>DESCRIPTION"
    "<C66>AMOUNT" 
    "<C74.5>ORDER#"   SKIP
    "<=5><R+1.5><C1.5>INVOICED"
    "<C57>EACH"
    "</B>" SKIP(1).

 
PUT "<FCourier New><P9>".

          

