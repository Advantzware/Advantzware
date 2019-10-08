 /* oe/rep/cocxprnt.i  */  
PUT "<FArial>"  SKIP
    "<P15><C+40><B>Packing Slip</B> " SKIP
    /*"<P15><C+40><B>" oe-bolh.bol-no "</B> " SKIP*/
    "<C1><#1><R+7><C+47><IMAGE#1=" ls-full-img1   SKIP /* APC package */  
    "<P12><C1><R-1><b> Prystup Packaging Products " SKIP
    "<C1> P.O. Box 1039" SKIP
    "<C1> Livingston, AL 35470 </b><FCourier New><P10>" SKIP

    "<=1><R+11><P10>"
    "<C7>BILLED TO:" SPACE(35) "SHIPPED TO:"  /*SKIP*/
    "<FCourier New>" SKIP
     cust.name                    AT 17
     v-ship-name                  AT 63
     cust.addr[1]                 AT 17
     v-ship-addr[1]               AT 63
     cust.addr[2]                 AT 17
     v-ship-addr[2]               AT 63
     v-cust-addr3                 AT 17
     v-ship-addr3                 AT 63 
    /* skip(1)
     v-po-no[1]                   at 33
     v-po-no[2]                   at 77
     skip(2)
     v-ord-date                   at 9
     oe-bolh.bol-date             at 23
     v-carrier-dscr               at 37 format "x(20)" 
     v-fob                        at 58
     v-ord-bol                    at 78
     skip(4)*/
     "<FArial><P15><=#3><P15>" SKIP
     "<=#3><R+3><C42><B> BOL#:" oe-bolh.bol-no FORMAT ">>>>>" "</B><P10>"
  
    "<|10><R21><C1><#4><FROM><R25><C78><RECT>" SKIP
     "<R23><C1><FROM><R23><C78><LINE>" SKIP    
     "<R21><C10><FROM><R25><C10><LINE>" SKIP
     "<R21><C20><FROM><R25><C20><LINE>" SKIP
     "<R21><C28><FROM><R25><C28><LINE>" SKIP
     "<R21><C42><FROM><R25><C42><LINE>" SKIP
     "<R21><C52><FROM><R25><C52><LINE>" SKIP
     "<R21><C58><FROM><R25><C58><LINE>" SKIP
     "<R21><C67><FROM><R25><C67><LINE>" SKIP
     "<FArial><=4><B>                             DATE            ORDER NO.     PURCHASE                TERMS            F.O.B         SALES         SHIPPED VIA   " SKIP 
     "<FArial><=4><R+1>    DATE               SHIPPED                                ORDER NO.                                                           PERSON      </b>" SKIP 
     "<FCourier New><=4><R+3> " TODAY SPACE(3) oe-bolh.bol-date SPACE(3) oe-boll.ord-no SPACE(3) oe-boll.po-no SPACE(1) v-terms SPACE(1)  v-fob SPACE(1)  "Prystup" SPACE(3)  oe-bolh.carrier      SKIP
     "<|10><R26><C1><#5><FROM><R60><C78><RECT>" SKIP 
     "<R28><C1><FROM><R28><C78><LINE>" SKIP 
     "<R26><C18><FROM><R60><C18><LINE>" SKIP
     "<R26><C56><FROM><R60><C56><LINE>" SKIP
     /*"<R22><C36><FROM><R24><C36><LINE>" SKIP*/
     "<R26><C65><FROM><R60><C65><LINE>" SKIP 
     "<FArial><=5><R+.5><B>        QUANTITY                                                         ITEM                                                           PRICE                 AMOUNT </B>" SKIP 
    .
PUT 
     "<FArial><R58.5><P15><C25><b>Thank you for your business</b>" SKIP
         "<C20><R60><P12><B> PLEASE REMIT TO :  MSC #669" SKIP
         "<C37><P12>PRYSTUP PACKAGING " SKIP
         "<C37><P12>P.O. BOX 830529  " SKIP
         "<C37><P12>BIRMINGHAM, AL 35283-0529 " SKIP .
        

        v-printline = v-printline + 20.
