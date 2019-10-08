/* oe/rep/invaccrd.i  */
PUT "<FArial>".
PUT "<C+25><#1>".
PUT "<=1>" SKIP.
PUT "<C1><#2>" .
PUT "<C1><#2><R+5><C+40><IMAGE#2=" ls-full-img1 SKIP  /* company image */ 
    /*"<P10><=2><R+4>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"*/
    "<P10><=2><R+3>"
    space(3) v-comp-add1  SKIP
    space(3) v-comp-add2 SKIP
    space(3) v-comp-add3 SKIP
    space(3) v-comp-add4 SKIP
    space(3) v-comp-add5 SKIP
    space(3) "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" lv-email FORMAT "X(48)" SKIP(1)
    "<FCourier New>"
    space(7) "Bill To:" SPACE(47) "Ship To:" SKIP
    SPACE(7) inv-head.cust-name v-shipto-name AT 63 skip
    SPACE(7) inv-head.addr[1]   v-shipto-addr[1] AT 63 SKIP
    SPACE(7) inv-head.addr[2]  v-shipto-addr[2] AT 63 SKIP
    SPACE(7) v-addr3   v-sold-addr3 AT 63 SKIP.
v-printline = v-printline + 18.
/*IF lv-display-comp THEN
   PUT "<=2><C3><R+2><FGCOLOR=" trim(lv-comp-color) + ">"
       "<=2><C2><R+3><P20><B>" lv-comp-name "<FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" .*/ 

PUT "</B><P10><R4><C50><#3><FROM><R10><C80><RECT><||3>" SKIP.
PUT "<R6><C50><FROM><R6><C80><LINE><||3>" SKIP
    "<R8><C50><FROM><R8><C80><LINE><||3>" SKIP
    "<R4><C62><FROM><R6><C62><LINE><||3>" SKIP
    "<R6><C65><FROM><R8><C65><LINE><||3>" SKIP
    "<R8><C65><FROM><R10><C65><LINE><||3>" SKIP.
        
PUT "<FArial><P12><=#3><R-2> <B>Invoice #: " inv-head.inv-no "</B><P10>                            Page: " string(PAGE-NUM - v-page-num,">>9") SKIP
    "<=#3> Customer ID             Contact"
    "<=#3><R+2> Telephone                        Fax" 
    "<=#3><R+4>     Invoice Date                 Cust PO# <FCourier New>"    
    "<=3><R+1> " inv-head.cust-no  space(7) cust.contact
    "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999" space(5) cust.fax
    "<=3><R+5> "  space(1) v-inv-date SPACE(7) v-po-ord  .

PUT "<|10><R19><C1><#4><FROM><R22><C80><RECT>" SKIP
    "<R20><C1><FROM><R20><C80><LINE>" SKIP    
    "<R19><C11><FROM><R22><C11><LINE>" SKIP
    "<R19><C22><FROM><R22><C22><LINE>" SKIP
    "<R19><C38><FROM><R22><C38><LINE>" SKIP
    "<R19><C52><FROM><R22><C52><LINE>" SKIP
    "<R19><C59><FROM><R22><C59><LINE>" SKIP
    "<R19><C72><FROM><R22><C72><LINE>" SKIP
    .
v-printline = v-printline + 3.
FIND FIRST sman WHERE sman.company = inv-head.company 
                  AND sman.sman = v-salesman
                  NO-LOCK NO-ERROR.
v-salesname = IF AVAIL sman THEN sman.sname ELSE "".

PUT "<FArial><=4>  Ship Date             FOB                    Ship Via                                 Terms                    Salesman    Salesman Name            BOL#" SKIP
     "<FCourier New><=4><R+2><B> " v-date-ship FORM "99/99/9999" space(2)
     v-fob FORM "x(12)" SPACE(1)
     v-shipvia FORM "x(20)" SPACE(1)
     xinv-head.terms-d FORM "x(15)" space(1) v-salesman FORM "x(8)"
     v-salesname FORM "x(15)"
     xinv-head.bol-no "</B>"
    SKIP.


PUT 
   /* "<|10><R28><C1><#5><FROM><R28><C80><LINE>" SKIP     
    "<|10><R31><C1><#6><FROM><R31><C80><LINE>" SKIP 
   */
   "<|10><R23><C1><#5><FROM><R25><C80><RECT>" SKIP    
    "<R23><C14><FROM><R25><C14><LINE>" SKIP
                "<R23><C27.5><FROM><R25><C27.5><LINE>" SKIP
                "<R23><C48><FROM><R25><C48><LINE>" SKIP
                "<R23><C62><FROM><R25><C62><LINE>" SKIP
                "<R23><C71><FROM><R25><C71><LINE>" SKIP 
    .   
/*puT "<FArial><=5><R+1>   Ordered     Shipped       B.O.      Item#                        Description                                              Price     UOM                 Amount" SKIP(1). */
PUT "<FArial><=5>  CUST PO#                Item# / CustPart#                                                                  QUANTITY                    Price                  " SKIP
    "   ORDER#                                                         DESCRIPTION                         Ordered     Shipped          (UOM)           AMOUNT" SKIP(1).

v-printline = v-printline + 3.
           
PUT "<FCourier New><B>".
