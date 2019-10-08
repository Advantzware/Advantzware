/* oe/rep/bolkni1.i */      
      put 
         "<FCourier New>"
           "<C3><R2><#1><R+8><C+45><IMAGE#1=" ls-full-img1 
          "<P14><R8><C46><B>Bill of lading #:" OE-BOLH.BOL-NO "</B><P10> "
        
            SKIP /* pacific package */             
        /* "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
         */
     /*    "<P10><=1><R+4>"  
         v-comp-add1 AT 8 SKIP
         v-comp-add2 AT 8  SKIP
         v-comp-add3 AT 8 SKIP
         v-comp-add4 AT 8 SKIP
         "www.pacificpackaging.ca" AT 8 SKIP(1)
               "<FCourier New>"  */
              "<R12><C1>"
               space(7) "Sold To:" SPACE(49) "Ship To:"  SKIP(1)
               SPACE(7) v-comp-name v-ship-name AT 65 skip
               SPACE(7) v-comp-addr[1] v-ship-addr[1] AT 65 SKIP
               SPACE(7) v-comp-addr[2] v-ship-addr[2] AT 65 SKIP
               SPACE(7) v-comp-addr3 v-ship-addr3 AT 65 SKIP
        "<R9><C46><#3>" SKIP
     /* "<P14><=#3>" /*<C-20><R-2> <B>Bill Of Lading</B>*/ "<P10>" SKIP
                "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP */
                "<=#3><R+1>Ship Date:" oe-bolh.bol-date        SKIP
             /*   "<=#3><R+2>" /*Ship Date:" oe-bolh.ship-date        */ SKIP
                 SKIP                                                      */
                "<||5><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
                "<R21><C1><FROM><R21><C81><LINE>" SKIP    
                "<R19><C12><FROM><R23><C12><LINE>" SKIP
                "<R19><C24><FROM><R23><C24><LINE>" SKIP      
                "<R19><C34><FROM><R23><C34><LINE>" SKIP
                "<R19><C55><FROM><R23><C55><LINE>" SKIP
                "<R19><C69><FROM><R23><C69><LINE>" SKIP
                "<=4><R+1>    Date          ORDER#        FOB          Carrier              Freight Terms        Weight" SKIP
                "<=4><R+3> " oe-bolh.bol-date /*v-po-no FORM "x(15)" */ space(4) v-ord-no space(7) v-fob space(1) carrier.dscr FORM "X(26)" space(1) v-frt-terms FORM "x(13)" SPACE(1) v-tot-wt FORM "->>,>>>,>>9" SKIP
                "<||5><R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
                "<R24><C12><FROM><R26><C12><LINE>" SKIP
                "<R24><C27><FROM><R26><C27><LINE>" SKIP
                "<R24><C48><FROM><R26><C48><LINE>" SKIP 
                "<R24><C54><FROM><R26><C54><LINE>" 
                "<R24><C68><FROM><R26><C68><LINE>" SKIP            
                "<R24><C77><FROM><R26><C77><LINE>"
            "<=5><R+1> Part#/Batch#   PO#/JOB#        Description                PAL    Cases-Quantity      Total P/C" SKIP(1)
            .
            v-printline = v-printline + 16.
