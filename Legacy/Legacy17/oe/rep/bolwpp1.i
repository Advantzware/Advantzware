/* oe/rep/bolwpp1.i */      

      put 
         "<FMS Mincho>"
          "<P14><R1><C52><B>Bill of lading #:" OE-BOLH.BOL-NO "</B><P10> " SKIP
          "<R2><C5>Manufactured by:"  "<R2><C30>Distributed by:"  SKIP
          "<C3><R3><#1><C+3><R+6><C+45><IMAGE#1=" ls-full-img1
           "<C30><R3><#1><C+3><R+6><C+45><IMAGE#1=" ls-full-img2 SKIP             
              "<R9><C1>" 
               space(4) "Sold To:" SPACE(49) "Ship To:"  SKIP
               SPACE(4) v-comp-name v-ship-name AT 65 skip
               SPACE(4) v-comp-addr[1] v-ship-addr[1] AT 65 SKIP
               SPACE(4) v-comp-addr[2] v-ship-addr[2] AT 65 SKIP
               SPACE(4) v-comp-addr3 v-ship-addr3 AT 65 SKIP
        "<R2><C46><#3>" SKIP

                "<||5><R14><C1><#4><FROM><R18><C80><RECT>" SKIP
                "<R16><C1><FROM><R16><C80><LINE>" SKIP    
                "<R14><C12><FROM><R18><C12><LINE>" SKIP
                "<R14><C23><FROM><R18><C23><LINE>" SKIP      
                "<R14><C43.3><FROM><R18><C43.3><LINE>" SKIP                
                "<R14><C51><FROM><R18><C51><LINE>" SKIP
                "<R14><C58.5><FROM><R18><C58.5><LINE>" 
                "<R14><C69><FROM><R18><C69><LINE>" SKIP
                /*"<=4><C54>NMFC#"*/
                "<=4><R+1>    Date           FOB               Carrier                  NMFC#      Class     Freight Terms     Weight" SKIP
                /*"<=4><R+2><C54>29280"*/
                "<=4><R+3> " oe-bolh.bol-date SPACE(3) /*v-po-no FORM "x(15)"  space(6) v-job-no*/ space(3) v-fob space(4) carrier.dscr FORM "x(28)" space(1) "152465" SPACE(6) "55" space(7) v-frt-terms space(5) v-tot-wt SKIP
                "<||5><R19><C1><#5><FROM><R21><C80><RECT>" SKIP    
                "<R19><C12><FROM><R21><C12><LINE>" SKIP
                "<R19><C23><FROM><R21><C23><LINE>" SKIP
                "<R19><C42><FROM><R21><C42><LINE>"                
                "<R19><C58><FROM><R21><C58><LINE>" SKIP 
                "<R19><C64><FROM><R21><C64><LINE>" 
                "<R19><C75.2><FROM><R21><C75.2><LINE>" SKIP            
                
            "<=5><R+1> Part# / Job#         PO#                Description              Tag#/Lot#           PAL    Unit-Quantity  Total" SKIP(1)
            .
            v-printline = v-printline + 18.
