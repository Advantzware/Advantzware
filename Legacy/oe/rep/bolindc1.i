/* oe/rep/bolindc1.i */      
      put "<FCourier New>"
          "<C3><R2><#1><R+8><C+45><IMAGE#1=" ls-full-img1 
          "<P14><R8><C46><B>Bill of lading #:" OE-BOLH.BOL-NO "</B><P10> "
            SKIP /* pacific package */             
              "<R12><C1>"
               space(7) "Sold To:" SPACE(39) "Ship To:"  SKIP(1)
               SPACE(7) v-comp-name v-ship-name AT 55 skip
               SPACE(7) v-comp-addr[1] v-ship-addr[1] AT 55 SKIP
               SPACE(7) v-comp-addr[2] v-ship-addr[2] AT 55 SKIP
               SPACE(7) v-comp-addr3 v-ship-addr3 AT 55 SKIP
             "<R9><C46><#3>" SKIP
                "<=#3><R+1>Ship Date:" oe-bolh.bol-date        SKIP
             /*   "<=#3><R+2>" /*Ship Date:" oe-bolh.ship-date        */ SKIP
                 SKIP                                                      */
                "<||5><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
                "<R21><C1><FROM><R21><C81><LINE>" SKIP    
                "<R19><C12><FROM><R23><C12><LINE>" SKIP
                "<R19><C24><FROM><R23><C24><LINE>" SKIP      
                "<R19><C34><FROM><R23><C34><LINE>" SKIP
                "<R19><C57><FROM><R23><C57><LINE>" SKIP
                /*"<R19><C69><FROM><R23><C69><LINE>" SKIP*/
                "<=4><R+1>    Date          ORDER#        FOB              Carrier                    Freight Terms    " SKIP
                "<=4><R+3> " oe-bolh.bol-date /*v-po-no FORM "x(15)" */ space(4) v-ord-no space(7) v-fob space(1) carrier.dscr space(1) v-frt-terms /*SPACE(1) v-tot-wt*/ SKIP
                "<||5><R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
                "<R24><C12><FROM><R26><C12><LINE>" SKIP
                "<R24><C25><FROM><R26><C25><LINE>" SKIP
                "<R24><C51><FROM><R26><C51><LINE>" SKIP 
                "<R24><C62><FROM><R26><C62><LINE>" 
                "<R24><C69><FROM><R26><C69><LINE>" SKIP            
                "<R24><C74><FROM><R26><C74><LINE>"
            "<=5> FG Item#  "
            "<=5><R+1> Your Part#       PO#               Description              Cases - Pack   Total Class  Weight" SKIP(1)
            .
            v-printline = v-printline + 16.
