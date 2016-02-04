/* ar/rep/invfibrex.i */
ASSIGN
  v-addr3a      = v-addr3
  v-sold-addr3a = v-sold-addr3
  v-addr-2 = YES.

PUT "<C1><R2><#1><R+7><C+33><IMAGE#1=" ls-full-img1 SKIP
    "<FLucida Console>"
    "<=1><R+5><P10>"
    SKIP(1)
    "15250 Don Julian Road" AT 3 SKIP
    "Industry, CA 91745" AT 3 SKIP
    "Phone: (626) 968-8503" AT 3 SKIP
    "Fax:   (626) 333-9621" AT 3 SKIP
    "www.fleetwood-fibre.com" AT 3 SKIP.

v-printline = v-printline + 19.

PUT "</B><P10><R5.5><C65><#3><FROM><R11.5><C80><RECT><||3>" SKIP
    "<R7.5><C65><FROM><R7.5><C80><LINE><||3>" SKIP
    "<R9.5><C65><FROM><R9.5><C80><LINE><||3>" SKIP
    "<P12><=#3><R4><B><C65>Invoice # " TRIM(STRING(ar-inv.inv-no)) "</B><P10>" SKIP
    "<=#3><C65> Invoice Date"
    "<=#3><R+2><C65> BOL #"
    "<=#3><R+4><C65> Customer ID</B>"    
    "<=3><R+1><C65> " SPACE(6) v-inv-date
    "<=3><R+3><C65> " SPACE(6) TRIM(STRING(lv-bol-no,">>>>>>>>"))
    "<=3><R+5><C65> " SPACE(6) ar-inv.cust-no
    "<B><C1><#2><R15><P10>" 
    "<C10>Sold To:<C52>Ship To:</B>" SKIP
    "<C10>" ar-inv.cust-name "<C52>" v-shipto-name SKIP.

IF TRIM(ar-inv.addr[1]) NE ""
  THEN PUT "<=2><R17><C10>" ar-inv.addr[1].
  ELSE DO:
     PUT "<=2><R17><C10>" ar-inv.addr[2].
     v-addr-2 = NO.
  END.

IF TRIM(v-shipto-addr[1]) NE ""
  THEN PUT "<=2><R17><C52>" v-shipto-addr[1] SKIP.
  ELSE DO:
   PUT "<=2><R17><C52>" v-shipto-addr[2] SKIP.
   ASSIGN v-shipto-addr[2] = "".
  END.

IF TRIM(ar-inv.addr[2]) NE "" AND v-addr-2 THEN
   PUT "<=2><R18><C10>" ar-inv.addr[2].
ELSE DO:
   PUT "<=2><R18><C10>" v-addr3a.
   ASSIGN v-addr3a = "".
END.

IF TRIM(v-shipto-addr[2]) NE ""
  THEN PUT "<=2><R18><C52>" v-shipto-addr[2] SKIP.
  ELSE DO:
   PUT "<=2><R18><C52>" v-sold-addr3a SKIP.
   ASSIGN v-sold-addr3a = "".
  END.

IF TRIM(v-addr3a) NE ""
  THEN PUT "<=2><R19><C10>" v-addr3a.
  
IF TRIM(v-sold-addr3a) NE ""
  THEN PUT "<=2><R19><C52>" v-sold-addr3 SKIP.
/*
    "<C10>" ar-inv.addr[1]   "<C42>" v-shipto-addr[1] SKIP
    "<C10>" ar-inv.addr[2]  "<C42>" v-shipto-addr[2] SKIP
    "<C10>" v-addr3 "<C42>" v-sold-addr3 SKIP.
*/

PUT    
    "<R22><C1><#4><FROM><R25><C82><RECT><||3>" SKIP
    "<R22><C13.5><FROM><R25><C13.5><LINE><||3>" SKIP
    "<R22><C24><FROM><R25><C24><LINE><||3>" SKIP
    "<R22><C42><FROM><R25><C42><LINE><||3>" SKIP
    "<R22><C58><FROM><R25><C58><LINE><||3>" SKIP
    "<R23><C1><FROM><R23><C82><LINE><||3>" SKIP.
    
v-printline = v-printline + 7.

PUT "<=4><B> CUSTOMER PO       FOB            SHIP VIA             TERMS                 SALES REP</B>" SKIP
    "<=4><R+1.5>" v-po-no FORM "X(15)"
    "<C12.5>" v-fob FORM "x(13)"
    "<C25.5>" v-shipvia FORM "x(20)"
    "<C43>" v-terms FORM "x(15)"
    "<C62.5>" v-salesman FORM "x(20)"
    SKIP
    "<R26><C1><#5><FROM><R29><C82><RECT><||3>" SKIP    
    "<R26><C9><FROM><R29><C9><LINE><||3>" SKIP
    "<R26><C16.5><FROM><R29><C16.5><LINE><||3>" SKIP
    "<R26><C23.5><FROM><R29><C23.5><LINE><||3>" SKIP
    "<R26><C36><FROM><R29><C36><LINE><||3>" SKIP
    "<R26><C55><FROM><R29><C55><LINE><||3>" SKIP
    "<R26><C59><FROM><R29><C59><LINE><||3>" SKIP
    "<R26><C69><FROM><R29><C69><LINE><||3>" SKIP
    "<R26><C73><FROM><R29><C73><LINE><||3>" SKIP
    "<P10><=5><B>" SPACE(29) "CUST PART #" SKIP
    "   QTY    SHIPPED  ORDER #   FG ITEM #          DESCRIPTION    P/C    PRICE    UOM   AMOUNT" SKIP
    " ORDERED  INVOICED  EST #      P.O. #" space(15) "LOT</B>" SKIP(1).

v-printline = v-printline + 9.

