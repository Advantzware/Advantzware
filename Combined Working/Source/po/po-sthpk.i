/* po/po-sthpk.i */  
  PUT
         "<FArial>"   SKIP
           "<P14><C+35> <B>Purchase Order</B> <P10>" v-change-ord SKIP
       "<C3><R2><#1><R+13><C+25>" "<IMAGE#1=" ls-full-img1  SKIP     
     /*     "<P10><=1><R+4>"
          "" /*v-comp-add1 */ AT 8 SKIP
          "" /*v-comp-add2 */ AT 8  SKIP
          "" /*v-comp-add3 */ AT 8 SKIP
          "" /*v-comp-add4 */ AT 8 SKIP
          "" /*"www.pacificpackaging.ca" */ AT 8 SKIP(1)*/
         "<FCourier New>"
         "Purchase Order To:" SPACE(30) "Ship To:"  SKIP
         SPACE(5) vend.name v-sname AT 45 skip
         SPACE(5) vend.add1 v-saddr[1] AT 45 SKIP
         SPACE(5) vend.add2 v-saddr[2] AT 45 SKIP
         SPACE(5) vend.city + " " + vend.state + " " + vend.zip FORM "x(35)"
                  v-scity + " " + v-sstate + " " + v-szip FORM "x(35)" AT 45 SKIP
         
       "<R4><C50><#3>" SKIP
       "<FArial><P14><=#3>" /*<C-20><R-2> <B>Purchase Order</B>*/  "<P10>" SKIP
          "<=#3><R+1><B><P12>PO #: " po-ord.po-no "</B><P10>" SKIP(1)
          "<=#3><R+2>Date: " po-ord.po-date        SKIP
          "<=#3><R+3>Changed Date: " po-ord.po-change-date SKIP
          "<=3><R+4>Date Required: " po-ord.due-date SKIP.
            /*
          WITH  FRAME po-top NO-BOX NO-LABEL STREAM-IO.

      VIEW FRAME po-top.
              */
      v-printline = v-printline + 15.    
      
      PUT "<|10><R23><C1><#4><FROM><R27><C80><RECT>" SKIP
      "<R25><C1><FROM><R25><C80><LINE>" SKIP    
      "<R23><C10><FROM><R27><C10><LINE>" SKIP
      "<R23><C29><FROM><R27><C29><LINE>" SKIP
      /*"<R19><C38><FROM><R23><C38><LINE>" SKIP*/
      "<R23><C49><FROM><R27><C49><LINE>" SKIP
      "<R23><C55><FROM><R27><C55><LINE>" SKIP
      "<R23><C72><FROM><R27><C72><LINE>" SKIP
      .
      PUT "<FArial><=4><R+1>  Buyer                 Contact                                     Terms                                        FOB        Ship Via                                Freight" SKIP
          "<FCourier New><=4><R+3> " po-ord.buyer    po-ord.contact FORM "x(25)" terms.dscr FORM "x(23)" po-ord.fob-code space(2) carrier.dscr FORM "x(20)" v-freight-dscr
          .
      
      PUT "<|10><R28><C1><#5><FROM><R30><C80><RECT>" SKIP    
             "<R28><C5><FROM><R30><C5><LINE>" SKIP
             "<R28><C15><FROM><R30><C15><LINE>" SKIP
             "<R28><C20><FROM><R30><C20><LINE>" SKIP 
             "<R28><C48><FROM><R30><C48><LINE>" SKIP
             "<R28><C55><FROM><R30><C55><LINE>" SKIP
             "<R28><C62><FROM><R30><C62><LINE>" SKIP
             "<R28><C68><FROM><R30><C68><LINE>" 
             "<R28><C72.5><FROM><R30><C72.5><LINE>" SKIP
             .

      PUT "<FArial><=5><R+1> Line         Quantity        UOM    Our Item/Description/Vendor Item                         Adder       Job#          Cost     UOM      Ext Cost" SKIP(1).
      PUT "<FCourier New>"          .
      v-printline = v-printline + 8.
