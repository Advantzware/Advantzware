 /* oe/rep/bolxprn2.i  */  
 
 
 /* oe/rep/bolmet1.i */      
   put 
      "<FCourier New>"
      "<C3><R2><#1><R+8><C+45><IMAGE#1=" ls-full-img1 
      "<P14><R8><C46><B>Bill of lading #:" w-fg-rctd.bol-no "</B><P10> "
      SKIP
      "<R12><C1>"
      space(7) "Sold To:" SPACE(49) "Ship To:"  SKIP(1)
      SPACE(7) v-comp-name v-ship-name AT 65 skip
      SPACE(7) v-comp-addr[1] v-ship-addr[1] AT 65 SKIP
      SPACE(7) v-comp-addr[2] v-ship-addr[2] AT 65 SKIP
      SPACE(7) v-comp-addr3 v-ship-addr3 AT 65 SKIP
      "<R9><C46><#3>" SKIP
      "<=#3><R+1>Ship Date:" TODAY /*oe-bolh.bol-date*/ SKIP
      "<||5><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
      "<R21><C1><FROM><R21><C81><LINE>" SKIP    
      "<R19><C12><FROM><R23><C12><LINE>" SKIP
      "<R19><C24><FROM><R23><C24><LINE>" SKIP      
      "<R19><C34><FROM><R23><C34><LINE>" SKIP
      "<R19><C55><FROM><R23><C55><LINE>" SKIP
      "<R19><C69><FROM><R23><C69><LINE>" SKIP
      "<=4><R+1>    Date          ORDER#        FOB          Carrier/SCAC         Freight Terms        Weight" SKIP
      "<=4><R+3> " TODAY space(4) v-ord-no space(9) v-fob space(1) carrier.dscr space(1) v-frt-terms SPACE(1) /*v-gross-wt*/ SKIP
      "<||5><R24><C1><#5><FROM><R28><C81><RECT>" SKIP    
      "<R26><C1><FROM><R26><C81><LINE>" SKIP    
      "<R24><C22><FROM><R28><C22><LINE>" SKIP
      "<R24><C48><FROM><R28><C48><LINE>" SKIP
      "<R24><C68><FROM><R28><C68><LINE>" SKIP           
      "<=5><R+1>    Manufacture Date                Trailer #                      Seal #          Ship From" SKIP
      "<=5><R+3> "  v-ship-from AT 93 SKIP
      "<||5><R29><C1><#6><FROM><R31><C81><RECT>" SKIP    
      "<R29><C17><FROM><R31><C17><LINE>" SKIP
      "<R29><C48><FROM><R31><C48><LINE>" SKIP 
      "<R29><C63><FROM><R31><C63><LINE>" 
      /*"<R29><C68><FROM><R31><C68><LINE>" SKIP   */         
      /*"<R29><C77><FROM><R31><C77><LINE>"*/
      "<=6><R+1>    Item#                 TAG #                              Quantities        Weight        " SKIP(1)
      .
       
   v-printline = v-printline + 21.
