/* ------------------------------------------- oe/rep/invcapitol.i 02071304 JAD */
/* PRINT INVOICE   Xprint form for Capitol                                      */
/* ---------------------------------------------------------------------------- */
/*****************************************************************************
        PUT "<FTimes New Roman>".
        PUT "<C+20><#1><B><P20>" skip
             space(24) v-comp-add1 FORM "x(50)"  "</B>" SKIP(1)
             "<P10>" space(65) v-comp-add2 FORM "x(60)" SKIP
             space(75) v-comp-add3 FORM "x(60)" SKIP
             space(62) v-comp-add4 FORM "x(60)" "<P10>"SKIP.
*****************************************************************************/             
PUT
  "<C20><#2><R+11><C+33><IMAGE#1=" ls-full-img1 SKIP.
 

PUT 
  "<=1>" SKIP
  "<C1><#2>" 
  "<P10><=2><R+11>"
  "<FCourier New><B>"
  "Bill To:" SPACE(45) "Ship To:" SKIP
  SPACE(7) inv-head.cust-name v-shipto-name AT 65 SKIP.

IF TRIM(inv-head.addr[1]) NE ""
  THEN PUT SPACE(7) inv-head.addr[1].
  ELSE DO:
   PUT SPACE(7) inv-head.addr[2].
   ASSIGN inv-head.addr[2] = "".
  END.

IF TRIM(v-shipto-addr[1]) NE ""
  THEN PUT v-shipto-addr[1] AT 65 SKIP.
  ELSE DO:
   PUT v-shipto-addr[2] AT 65 SKIP.
   ASSIGN v-shipto-addr[2] = "".
  END.

IF TRIM(inv-head.addr[2]) NE ""
  THEN PUT SPACE(7) inv-head.addr[2].
  ELSE DO:
   PUT SPACE(7) v-addr3.
   ASSIGN v-addr3 = "".
  END.

IF TRIM(v-shipto-addr[2]) NE ""
  THEN PUT v-shipto-addr[2] AT 65 SKIP.
  ELSE DO:
   PUT v-sold-addr3 AT 65 SKIP.
   ASSIGN v-sold-addr3 = "".
  END.

IF TRIM(v-addr3) NE ""
  THEN PUT SPACE(7) v-addr3.
  
IF TRIM(v-sold-addr3) NE ""
  THEN PUT v-sold-addr3 AT 65 SKIP.

  PUT  "</B>" SKIP.

ASSIGN  v-printline = v-printline + 15.

PUT "<|10><R7><C53><#3><FROM><R11><C78><RECT>" SKIP.

PUT 
  "<R10><C53><FROM><R10><C78><LINE>" SKIP
  "<R10><C65><FROM><R11><C65><LINE>" SKIP.  

        
PUT "<FArial><P12><=#3><R-2> <P10>" SKIP
    "<=#3><B>                      INVOICE#                    " SKIP
    "<=#3><R+1><P14>                 "inv-head.inv-no "</B><P10>"
    "<=#3><R+3>              DATE               " v-inv-date "<FCourier New>"    
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
  WHERE sman.company EQ inv-head.company 
    AND sman.sman    EQ v-salesman NO-ERROR.

ASSIGN v-salesname = IF AVAIL sman 
                       THEN sman.sname ELSE "".

PUT 
/*   "<FArial><=4><B>  Ship Date             FOB                    Ship Via                                 Terms                    Salesman    Salesman Name            BOL#" SKIP */
  "<FArial><=4><B>  Ship Date             FOB                    Ship Via                                 Terms                          Sales Person                      BOL#" SKIP
  "<FCourier New><=4><R+2></B> " v-date-ship FORMAT "99/99/9999" 
 SPACE(2)
  v-fob FORMAT "x(12)" 
 SPACE(1)
  v-shipvia FORMAT "x(20)" 
 SPACE(1)
  xinv-head.terms-d FORMAT "x(15)" 
 SPACE(6)
  v-salesname FORMAT "x(15)"
  v-bol-no "</B>"
 SKIP.

/*
PUT 
   /* "<|10><R28><C1><#5><FROM><R28><C80><LINE>" SKIP     
    "<|10><R31><C1><#6><FROM><R31><C80><LINE>" SKIP 
   */
   "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
    "<R25><C13><FROM><R27><C13><LINE>" SKIP
                "<R25><C35><FROM><R27><C35><LINE>" SKIP
                "<R25><C52><FROM><R27><C52><LINE>" SKIP
                "<R25><C61><FROM><R27><C61><LINE>" SKIP
                "<R25><C72><FROM><R27><C72><LINE>" SKIP 
    .   
puT "<FArial><=5><R+1>   Ordered     Shipped       B.O.      Item#                        Description                                              Price     UOM                 Amount" SKIP(1). */

PUT 
   "<|10><B><R25><C1><#5><FROM><R63><C80.75><RECT>" SKIP 
   "<C1><R27><FROM><R27><C80.75><LINE>"             SKIP 
   "<C61><R59><FROM><R59><C80.75><LINE>"            SKIP

    "<R25><C13><FROM><R61><C13><LINE>"           SKIP 
    "<R25><C35><FROM><R60><C35><LINE>"           SKIP 
    "<R27><C43.5><FROM><R60><C43.5><LINE>"       SKIP 
    "<R25><C52><FROM><R60><C52><LINE>"           SKIP 
    "<R25><C61><FROM><R61><C61><LINE>"           SKIP 
    "<R25><C72><FROM><R59><C72><LINE>"           SKIP 
    "</B>".   


PUT "<FArial><=5><B>  CUST PO#                      CUSTOMER PART #                         QUANTITY                                              Price                  " SKIP
    "   ORDER#                           DESCRIPTION                        Shipped      Invoiced            P/C                    (UOM)         AMOUNT"    
                                       
    "</B>"SKIP(1).

ASSIGN v-printline = v-printline + 3.
           
PUT "<FCourier New></B>".
