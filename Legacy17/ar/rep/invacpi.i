/* ------------------------------------------ ar/rep/invacpi.i 06050916 GDM  */
/* INVOICE PRINT  Program for N-K-1-INVPRINT = ACPI                          */
/* ------------------------------------------------------------------------- */

PUT "<FTimes New Roman>".
PUT "<C3><R4><#1><P10> "
    /*
    SPACE(75) v-comp-add3 FORM "x(60)" SKIP
    SPACE(62) v-comp-add4 FORM "x(60)" "<P10>" SKIP  
    */
    "<R+7><C+75><IMAGE#1=" ls-full-img1 SKIP  /* image */ .

PUT "<P10><=1><R+5.5>"
    "<FCourier New>"
    SPACE(10) v-comp-add1 FORMAT "x(30)" 
     "<C53>" v-comp-add4 FORMAT "x(25)" 
    SKIP.

IF TRIM(v-comp-add3) NE "" 
  THEN PUT SPACE(12.4) v-comp-add2 FORMAT "x(30)" 
           "<C53>" v-comp-add5 FORMAT "x(25)" 
           SKIP
          SPACE(12.4) v-comp-add3 FORMAT "x(30)" 
          SKIP.
  ELSE PUT SPACE(12.4) v-comp-add2 FORMAT "x(30)" 
           "<C53>" v-comp-add5 FORMAT "x(25)" 
           SKIP.
PUT "<=1>". 

PUT "<C1><#2>" 
    "<P10><=2><R+6>"
    "<FCourier New>"
   SKIP(3)
    SPACE(12) "BILL TO:" SPACE(43) "SHIP TO:" SKIP
    SPACE(12) ar-inv.cust-name v-shipto-name    AT 64 SKIP
    SPACE(12) ar-inv.addr[1]   v-shipto-addr[1] AT 64 SKIP
    SPACE(12) ar-inv.addr[2]   v-shipto-addr[2] AT 64 SKIP
    SPACE(12) v-addr3            v-sold-addr3     AT 64 "</B>" SKIP.

v-printline = v-printline + 15.

PUT "<|10><R7><C53><#3><FROM><R9><C80><RECT>" SKIP.

PUT "<R8><C53><FROM><R8><C80><LINE>" SKIP
    "<R7><C65><FROM><R9><C65><LINE>" SKIP .
/*"<R8><C65><FROM><R10><C65><LINE>" SKIP.*/

        
PUT "<FArial><P12><=#3><R-2> <P10>" ip-copy-title FORM "x(20)" SKIP
    "<=#3>          INVOICE#                    " ar-inv.inv-no
    "<=#3><R+1>              DATE               " v-inv-date "<FCourier New>"    
    SKIP(1)
    .

PUT "<|10><R21><C1><#4><FROM><R24><C80><RECT>" SKIP
    "<R22><C1><FROM><R22><C80><LINE>" SKIP    
    "<R21><C11><FROM><R24><C11><LINE>" SKIP
    "<R21><C22><FROM><R24><C22><LINE>" SKIP
    "<R21><C38><FROM><R24><C38><LINE>" SKIP
    "<R21><C55><FROM><R24><C55><LINE>" SKIP
 /* "<R21><C59><FROM><R24><C59><LINE>" */ SKIP 
    "<R21><C65><FROM><R24><C65><LINE>" SKIP
    .

v-printline = v-printline + 3.
/*
FIND FIRST sman WHERE sman.company = ar-inv.company 
                  AND sman.sman = v-salesman
                  NO-LOCK NO-ERROR.
v-salesname = IF AVAIL sman THEN sman.sname ELSE "".
*/

PUT "<FArial><=4>  SHIP DATE             FOB                      SHIP VIA                               TERMS                      SALESMAN                  BOL#" SKIP
    "<FCourier New><=4><R+2><B> " 
    v-date-ship    FORMAT "99/99/9999" 
   SPACE(2)
    v-fob          FORMAT "x(12)" 
   SPACE(1)
    v-shipvia      FORMAT "x(20)" 
   SPACE(1)
    ar-inv.terms-d FORMAT "x(20)" 
   SPACE(1)
    v-salesman     FORMAT "x(10)" 
   SPACE(3)
    lv-bol-no      FORMAT ">>>>>>>" "</B>"
    SKIP.


PUT 
   /* "<|10><R28><C1><#5><FROM><R28><C80><LINE>" SKIP     
    "<|10><R31><C1><#6><FROM><R31><C80><LINE>" SKIP 
   */
    "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
    "<R25><C13><FROM><R27><C13><LINE>" SKIP
    "<R25><C25><FROM><R27><C25><LINE>"     
    "<R25><C52><FROM><R27><C52><LINE>" SKIP
    "<R25><C60><FROM><R27><C60><LINE>" SKIP
    "<R25><C65><FROM><R27><C65><LINE>" 
    "<R25><C71><FROM><R27><C71><LINE>" SKIP 

    .   
/*puT "<FArial><=5><R+1>   Ordered     Shipped       B.O.      Item#                        Description                                              Price     UOM                 Amount" SKIP(1). */
PUT "<FArial><=5>  CUST PO#              CUST PART#                                                             <C52.5> INVOICED              <C62> P       PRICE                  " SKIP
    "  OUR ORDER#        ITEM#                                     DESCRIPTION                             SHIPPED        C       (UOM)        AMOUNT" SKIP(1).

v-printline = v-printline + 3.
           
PUT "<FCourier New><B>".
