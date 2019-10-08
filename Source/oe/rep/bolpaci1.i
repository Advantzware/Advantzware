/* oe/rep/bolpaci1.i */      
      
      put 
         "<FArial>"  SKIP
          "<P14><C+40><B>Bill Of Lading</B> " SKIP
          "<C1><#1><R+5><C+25><IMAGE#1=" ls-full-img1 SKIP /* pacific package */             
        /* "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
         */
         "<P10><=1><R+4>"  
         v-comp-add1 AT 8 SKIP
         v-comp-add2 AT 8  SKIP
         v-comp-add3 AT 8 SKIP
         v-comp-add4 AT 8 SKIP
         "www.actionbox.ca" AT 8 SKIP(1)
               "<FCourier New>"
               "Sold To:" SPACE(30) "Ship To:"  SKIP
               SPACE(5) v-comp-name v-ship-name AT 45 skip
               SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
             SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
               SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP
        "<R5><C50><#3>" SKIP
        "<FArial><P14><=#3>" /*<C-20><R-2> <B>Bill Of Lading</B>*/ "<P10>" SKIP
                "<=#3><B>BOL #: " oe-bolh.bol-no "</B>"
                string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(15)" AT 65  SKIP(1)
                "<=#3><R+2>Date:" oe-bolh.bol-date        SKIP
                "<=#3><R+3>" /*Ship Date:" oe-bolh.ship-date        */ SKIP
                 SKIP     
                "<|10><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
                "<R21><C1><FROM><R21><C81><LINE>" SKIP    
                "<R19><C12><FROM><R23><C12><LINE>" SKIP
                "<R19><C25><FROM><R23><C25><LINE>" SKIP      
                "<R19><C34><FROM><R23><C34><LINE>" SKIP
                "<R19><C46><FROM><R23><C46><LINE>" SKIP
                "<R19><C66><FROM><R23><C66><LINE>" SKIP
                "<FArial><=4><R+1>    Date                    PO#                               JOB#                 FOB                  Carrier                                                 Freight Terms" SKIP
                "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) v-po-no FORM "x(15)" space(2) v-job-no v-fob space(3) carrier.dscr v-frt-terms SKIP
                "<|10><R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
                "<R24><C13><FROM><R26><C13><LINE>" SKIP
                "<R24><C27><FROM><R26><C27><LINE>" SKIP
                "<R24><C49><FROM><R26><C49><LINE>" SKIP 
                "<R24><C62><FROM><R26><C62><LINE>" SKIP
                "<R24><C73><FROM><R26><C73><LINE>" SKIP            
            "<FArial><=5><R+1> Part#                        Finished Good#           Description                                        Unit-Quantity           Partial/Complete    Weight" "<FCourier New>" SKIP(1)
            .
            v-printline = v-printline + 16.
