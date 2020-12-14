/* ------------------------------------------ oe/rep/invcapcin.i  GDM */
/* INVOICE PRINT  Program for N-K-1-INVPRINT = CapCityIN                        */
/* ------------------------------------------------------------------------- */

ASSIGN  cSoldName   = IF cSoldID NE "" THEN cSoldName ELSE inv-head.cust-name
        v-blt-addr1 = IF cSoldID NE "" THEN cSoldAddress[1]  ELSE inv-head.addr[1]
        v-blt-addr2 = IF cSoldID NE "" THEN cSoldAddress[2]  ELSE inv-head.addr[2]
        v-blt-addr3 = IF cSoldID NE "" THEN (cSoldCity + ", " + cSoldState + "  " + cSoldZip) ELSE v-addr3 
                  
        v-sh-addr1 = v-shipto-addr[1]
        v-sh-addr2 = v-shipto-addr[2]
        v-sh-addr3 = v-sold-addr3.

PUT "<FArial>".
PUT "<C2><R3><#1><R+10><C+50><IMAGE#1=" ls-full-img1  .
PUT "<C+25><#1>".
PUT "<=1>" SKIP.
PUT "<C1><#2><P10>"
     "<FCourier New>"
        SPACE(7) "Sold To:" SPACE(43) "Ship To:"        SKIP
        SPACE(7) cSoldName v-shipto-name AT 59 SKIP.

IF TRIM(v-blt-addr1) NE "" 
  THEN PUT SPACE(7) v-blt-addr1.
  ELSE DO:
   IF TRIM(v-blt-addr2) NE "" 
     THEN DO:
      PUT SPACE(7) v-blt-addr2.
      ASSIGN v-blt-addr2 = "".
   END.
   ELSE DO:
     PUT SPACE(7) v-blt-addr3.
     ASSIGN v-blt-addr3 = "".
   END.
  END.

IF TRIM(v-sh-addr1) NE "" 
  THEN PUT v-sh-addr1 AT 59 SKIP.
  ELSE DO:
    IF TRIM(v-sh-addr2) NE "" 
      THEN DO: 
        PUT v-sh-addr2 AT 59 SKIP.
        ASSIGN v-sh-addr2 = "".
    END.
    ELSE 
      IF TRIM(v-sh-addr3) NE "" THEN DO: 
        PUT v-sh-addr3 AT 59 SKIP.
        ASSIGN v-sh-addr3 = "".
      END.
      ELSE PUT SKIP.
  END.    

IF TRIM(v-blt-addr2) NE "" 
  THEN PUT SPACE(7) v-blt-addr2.
  ELSE DO:
   IF TRIM(v-blt-addr3) NE "" 
     THEN DO:
      PUT SPACE(7) v-blt-addr3.
      ASSIGN v-blt-addr3 = "".
   END.   
  END.

IF TRIM(v-sh-addr2) NE "" 
  THEN PUT v-sh-addr2 AT 59 SKIP.
  ELSE DO:
    IF TRIM(v-sh-addr3) NE "" 
      THEN DO: 
        PUT v-sh-addr3 AT 59 SKIP.
        ASSIGN v-sh-addr3 = "".
    END.    
    ELSE PUT SKIP.
  END.

  IF TRIM(v-blt-addr3) NE "" 
    THEN PUT SPACE(7) v-blt-addr3.
    
  IF TRIM(v-sh-addr3) NE "" 
    THEN PUT v-sh-addr3 AT 59 SKIP.
    ELSE PUT SKIP.
    
ASSIGN v-printline = v-printline + 18.


PUT "</B><P10><R4><C50><#3><FROM><R10><C80><RECT><||3>" SKIP.
PUT "<R6><C50><FROM><R6><C80><LINE><||3>" SKIP
    "<R8><C50><FROM><R8><C80><LINE><||3>" SKIP
    "<R4><C59><FROM><R6><C59><LINE><||3>" SKIP
    "<R6><C65><FROM><R8><C65><LINE><||3>" SKIP.
        
PUT "<FArial><P12><=#3><R-2> <B>Invoice#: " inv-head.inv-no FORMAT ">>>>>>>" "</B><P10>                            Page: " string(PAGE-NUM - v-page-num,">>9") SKIP
    "<=#3> Customer ID           Contact"
    "<=#3><R+2> Telephone                        Fax"
    "<=#3><R+4>" SPACE(30) "Invoice Date <FCourier New>"    
    "<=3><R+1> " inv-head.cust-no  SPACE(3) cust.contact
    "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999" space(5) cust.fax
    "<=3><R+5> " SPACE(12) v-inv-date
    "<R20><C1><#4><FROM><R23><C80><RECT><||3>" SKIP
    "<R21><C1><FROM><R21><C80><LINE><||3>"  SKIP    
    "<R20><C11><FROM><R23><C11><LINE><||3>" SKIP
    "<R20><C22><FROM><R23><C22><LINE><||3>" SKIP
    "<R20><C38><FROM><R23><C38><LINE><||3>"  SKIP
    "<R20><C55><FROM><R23><C55><LINE><||3>" SKIP.

ASSIGN
   v-printline = v-printline + 6.

DO v-icnt = 1 TO 2:
  FIND FIRST sman NO-LOCK 
    WHERE sman.company EQ inv-head.company 
      AND sman.sman    EQ v-salesman[v-icnt] NO-ERROR.

ASSIGN v-salesman[v-icnt] = IF AVAIL sman 
                         THEN sman.sname ELSE v-salesman[v-icnt].
END.

IF TRIM(v-salesman[1]) EQ TRIM(v-salesman[2]) 
  THEN v-salesman[2] = "".   

PUT "<FArial><=4>     Ship Date               FOB                        Ship Via                           Terms                                    Sales Person Name" SKIP
     "<FCourier New><=4><R21.5> "
        v-date-ship       FORMAT "99/99/9999" SPACE(2)
        v-fob             FORMAT "x(12)"      SPACE(1)
        v-shipvia         FORMAT "x(20)"      SPACE(1)
        xinv-head.terms-d FORMAT "x(15)"      SPACE(11) 
    "<R21.4>"
     v-salesman[1]        FORMAT "x(20)"      SKIP
     "<C54><R22>  " 
     SPACE(5)
     v-salesman[2]         FORMAT "x(20)"
      
     /*xinv-head.bol-no*/
    SKIP.

PUT "<R23><C1><#5><FROM><R25><C80><RECT><||3>" SKIP    
    "<R23><C9.3><FROM><R25><C9.3><LINE><||3>" SKIP
    "<R23><C17.5><FROM><R25><C17.5><LINE><||3>" SKIP
    "<R23><C23.5><FROM><R25><C23.5><LINE><||3>" SKIP
    "<R23><C37><FROM><R25><C37><LINE><||3>" SKIP
    "<R23><C60.2><FROM><R25><C60.2><LINE><||3>" SKIP
    /*"<R23><C68><FROM><R25><C68><LINE><||3>" SKIP */
    "<R23><C72><FROM><R25><C72><LINE><||3>" SKIP.
PUT
    "<FArial><=5>"                              
    SPACE(1) "Qty Ordered"   SPACE(1) "Qty Invoiced"         
    SPACE(18) "Cust Part #" SPACE(17) "Po#                /                        Line# " SPACE(1)                
    SPACE(12) "Price "       SPACE(2)  "UOM"         SPACE(7)  "Amount" SKIP
    SPACE(3) "Order# "      SPACE(7)  "Qty Shipped"    SPACE(4)  "BOL #"
    SPACE(4) "FG Item #" SPACE(19) "Description" SKIP(1).

v-printline = v-printline + 7.
           

PUT "<FCourier New>".
