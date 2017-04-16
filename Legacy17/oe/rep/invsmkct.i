/* ------------------------------------------ oe/rep/invsmkct.i 04270902 GDM */
/* INVOICE PRINT  Program for N-K-1-INVPRINT = Simkct                        */
/* ------------------------------------------------------------------------- */

ASSIGN  v-blt-addr1 = inv-head.addr[1]
        v-blt-addr2 = inv-head.addr[2]
        v-blt-addr3 = v-addr3
                  
        v-sh-addr1 = v-shipto-addr[1]
        v-sh-addr2 = v-shipto-addr[2]
        v-sh-addr3 = v-sold-addr3.

PUT "<FArial>".
PUT "<C+25><#1>".
PUT "<=1>" SKIP.
PUT "<C1><#2>"
    "<P10><=2><R+4>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
    "<P10><=2><R+5>"
    space(3) v-comp-add1  SKIP
    space(3) v-comp-add2 SKIP
    space(3) v-comp-add3 SKIP
    space(3) v-comp-add4 SKIP
    space(3) v-comp-add5 SKIP
    space(3) "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" lv-email FORMAT "X(48)" SKIP(1)
     "<FCourier New>"
        SPACE(7) "Bill To:" SPACE(47) "Ship To:"        SKIP
        SPACE(7) inv-head.cust-name v-shipto-name AT 63 SKIP.

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
  THEN PUT v-sh-addr1 AT 63 SKIP.
  ELSE DO:
    IF TRIM(v-sh-addr2) NE "" 
      THEN DO: 
        PUT v-sh-addr2 AT 63 SKIP.
        ASSIGN v-sh-addr2 = "".
    END.
    ELSE 
      IF TRIM(v-sh-addr3) NE "" THEN DO: 
        PUT v-sh-addr3 AT 63 SKIP.
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
  THEN PUT v-sh-addr2 AT 63 SKIP.
  ELSE DO:
    IF TRIM(v-sh-addr3) NE "" 
      THEN DO: 
        PUT v-sh-addr3 AT 63 SKIP.
        ASSIGN v-sh-addr3 = "".
    END.    
    ELSE PUT SKIP.
  END.

  IF TRIM(v-blt-addr3) NE "" 
    THEN PUT SPACE(7) v-blt-addr3.
    
  IF TRIM(v-sh-addr3) NE "" 
    THEN PUT v-sh-addr3 AT 63 SKIP.
    ELSE PUT SKIP.
    
ASSIGN v-printline = v-printline + 18.

IF lv-display-comp THEN
   PUT "<=2><C3><R+2><FGCOLOR=" trim(lv-comp-color) + ">"
       "<=2><C2><R+3><P20><B>" lv-comp-name "<FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" . 

PUT "</B><P10><R4><C50><#3><FROM><R10><C80><RECT><||3>" SKIP.
PUT "<R6><C50><FROM><R6><C80><LINE><||3>" SKIP
    "<R8><C50><FROM><R8><C80><LINE><||3>" SKIP
    "<R4><C62><FROM><R6><C62><LINE><||3>" SKIP
    "<R6><C65><FROM><R8><C65><LINE><||3>" SKIP
/*     "<R8><C65><FROM><R10><C65><LINE><||3>" SKIP */
    .
        
PUT "<FArial><P12><=#3><R-2> <B>Invoice#: " inv-head.inv-no "</B><P10>                            Page: " string(PAGE-NUM - v-page-num,">>9") SKIP
/*     "<FArial><P12><=#3><R-2><P10>" SPACE(60) "Page: " string(PAGE-NUM - v-page-num,">>9") SKIP */
    "<=#3> Customer ID             Contact"
    "<=#3><R+2> Telephone                        Fax" 
/*     "<=#3><R+4>          Invoice #                        Invoice Date <FCourier New>" */
    "<=#3><R+4>" SPACE(30) "Invoice Date <FCourier New>"    
    "<=3><R+1> " inv-head.cust-no  SPACE(7) cust.contact
    "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999" space(5) cust.fax
    "<=3><R+5> " /*SPACE(3) inv-head.inv-no*/ SPACE(12) v-inv-date .

    
PUT "<R22><C1><#4><FROM><R25><C80><RECT><||3>" SKIP
    "<R23><C1><FROM><R23><C80><LINE><||3>"  SKIP    
    "<R22><C11><FROM><R25><C11><LINE><||3>" SKIP
    "<R22><C22><FROM><R25><C22><LINE><||3>" SKIP
    "<R22><C38><FROM><R25><C38><LINE><||3>"  SKIP
    "<R22><C55><FROM><R25><C55><LINE><||3>" SKIP
    .

v-printline = v-printline + 6+.

v-icnt = 0.

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
     "<FCourier New><=4><R23.5> "
        v-date-ship       FORMAT "99/99/9999" SPACE(2)
        v-fob             FORMAT "x(12)"      SPACE(1)
        v-shipvia         FORMAT "x(20)"      SPACE(1)
        xinv-head.terms-d FORMAT "x(15)"      SPACE(4) 
    "<R23>"
     v-salesman[1]        FORMAT "x(20)"             SKIP
     "<C54><R24>  "     
     v-salesman[2]         FORMAT "x(20)"
      
     /*xinv-head.bol-no*/
    SKIP.

PUT "<R26><C1><#5><FROM><R29><C80><RECT><||3>" SKIP    
    "<R26><C9><FROM><R29><C9><LINE><||3>" SKIP
    "<R26><C17><FROM><R29><C17><LINE><||3>" SKIP
    "<R26><C23.5><FROM><R29><C23.5><LINE><||3>" SKIP
    "<R26><C36><FROM><R29><C36><LINE><||3>" SKIP
/*     "<R26><C55.5><FROM><R29><C55.5><LINE><||3>" SKIP */
    "<R26><C59><FROM><R29><C59><LINE><||3>" SKIP
    "<R26><C68><FROM><R29><C68><LINE><||3>" SKIP
    "<R26><C72><FROM><R29><C72><LINE><||3>" SKIP.
PUT
    "<FArial><=5>" SPACE(65) "FG Item #" SKIP                             
    SPACE(6) "Qty"          SPACE(16) "Qty"         SPACE(10) "Order#" 
    SPACE(11) "Cust Part #" SPACE(24) "Description" SPACE(20)                
    SPACE(17) "Price"       SPACE(6)  "UOM"         SPACE(6)  "Amount" SKIP
    SPACE(3) "Shipped"      SPACE(8)  "Invoiced"    SPACE(6)  "BOL #"
    SPACE(13) "P.O. #"  SKIP(1).

v-printline = v-printline + 7.
           

PUT "<FCourier New>".
