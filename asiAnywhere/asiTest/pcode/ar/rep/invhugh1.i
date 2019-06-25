/*ar/rep/invhugh1.i  */

   PUT "<FArial>".
   PUT "<C+25><#1>".
   PUT "<=1>" SKIP.
   PUT 
      "<C1><#2>" 
      "<P10><=2><R+12>" SKIP
      "<FCourier New><B>"
      "Bill To:  </B>"  ar-inv.cust-name  
      "<B>Ship To:  </B>" AT 65 v-shipto-name FORMAT "X(25)" SKIP
      SPACE(10) ar-inv.addr[1]  
      v-shipto-addr[1]           AT 54    SKIP
      SPACE(10) ar-inv.addr[2]   
      v-shipto-addr[2]           AT 54    SKIP
      SPACE(10) v-addr3        
      v-sold-addr3               AT 54    SKIP(1).

   v-printline = v-printline + 15.
   
   PUT
      "<|10><R8><C50><#3><FROM><R12><C80><RECT>" SKIP.
   
   PUT 
      "<R8><C62><FROM><R10><C62><LINE>" SKIP      /* vertical line */
      "<R10><C50><FROM><R10><C80><LINE>" SKIP     /* horizontal line */
      "<R10><C62><FROM><R12><C62><LINE>" SKIP.    /* vertical line */
     
   PUT 
      "<FArial><P12><=#3><R-3> <B>Invoice#: " ar-inv.inv-no  
      "             <=#3><R-2> <B>Invoice Date: " v-inv-date "</B><P10>" SKIP
      "<=#3><R+0><B> Customer ID         Telephone" 
      "<=#3><R+2><B> Contact                 Customer Email <FCourier New></B>"    
      "<=3><R+1> " ar-inv.cust-no SPACE(6) cust.area-code + cust.phone FORMAT "(999)999-9999" 
      "<=3><R+3> " cust.contact   FORMAT "X(13)" SPACE(1) TRIM(cust.email) FORMAT "X(20)". 
    
   PUT 
      "<|10><R23><C1><#4><FROM><R25><C80><RECT>" SKIP
      "<R23><C10.5><FROM><R25><C10.5><LINE>" SKIP
      "<R23><C20.5><FROM><R25><C20.5><LINE>" SKIP
      "<R23><C35><FROM><R25><C35><LINE>" SKIP
      "<R23><C52><FROM><R25><C52><LINE>" SKIP
      "<R23><C66><FROM><R25><C66><LINE>" SKIP
      "<R23><C71.2><FROM><R25><C71.2><LINE>" SKIP.
  
   v-printline = v-printline + 5.

   PUT 
      "<FArial><=4><R+0><B> Ship Date        FOB                         Ship Via              Terms                                  Salesman Name       Pallets        BOL#</B>" SKIP
      "<FCourier New><=4><R+1> " v-date-ship FORM "99/99/9999" space(1)
      v-fob FORM "x(11)" SPACE(1)
      v-shipvia FORM "x(16)" SPACE(1)
      ar-inv.terms-d FORM "x(20)" space(1) 
      v-salesman FORMAT "X(14)" SPACE(1) 
      v-tot-pallets FORM ">>>,>>9" 
      v-bol-no SKIP.

   PUT 
      "<|10><R26><C1><#5><FROM><R28><C80><RECT>" SKIP   
      "<R26><C10.5><FROM><R28><C10.5><LINE>" SKIP
      "<R26><C20.5><FROM><R28><C20.5><LINE>" SKIP
      "<R26><C27><FROM><R28><C27><LINE>" SKIP
      "<R26><C35><FROM><R28><C35><LINE>" SKIP
      "<R26><C52><FROM><R28><C52><LINE>" SKIP
      "<R26><C56.7><FROM><R28><C56.7><LINE>" SKIP
      "<R26><C66><FROM><R28><C66><LINE>" SKIP
      "<R26><C71.2><FROM><R28><C71.2><LINE>" SKIP.

   PUT "<FArial><=5><R+0><B>                            Quantities                     Your PO#    Item                                   " SKIP(1).
   PUT "<FArial><=5><R+1><B>     Ordered           Shipped       Invoiced  Our Ord#    Description                          Ln#           Price        UOM         Amount</B>" SKIP(1).

   v-printline = v-printline + 4.
           
   /*  page.   ???*/
   PUT "<FCourier New>".

