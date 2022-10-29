/* oe/rep/bolccc1.i */      

     IF NOT lv-print-img THEN
       put 
         "<FMS Mincho>"
          "<P14><R4><C52><B>Bill of lading #:" OE-BOLH.BOL-NO "</B><P10> "
          "<C1><R2><#1><R+8><C+42>" "<IMAGE#1=" ls-full-img1  SKIP    .
      IF lv-print-img THEN
          PUT  "<FMS Mincho>"
          "<P14><R3><C52><B>Bill of lading #:" OE-BOLH.BOL-NO "</B><P10> " SKIP
          "<R3><C5>Manufactured by:"  "<R3><C30>Distributed by:"  SKIP
          "<C3><R4><#1><C+3><R+6><C+45><IMAGE#1=" ls-full-img1
           "<C30><R4><#1><C+3><R+6><C+45><IMAGE#1=" ls-full-img2 SKIP .

      PUT
              "<R10><C1>" 
               "<C4>" "Sold To:"            "<C43>" "Ship To:" SKIP
               "<C4>" v-comp-name           "<C46>" v-ship-name SKIP
               "<C4>" v-comp-addr[1]        "<C46>" v-ship-addr[1] SKIP
               "<#3><C4>" v-comp-addr[2]    "<C46>" v-ship-addr[2] SKIP.
               
               IF v-comp-addr[3] NE "" OR v-ship-addr[3] NE "" THEN
                    PUT "<#3><C4>" v-comp-addr[3]   "<C46>" v-ship-addr[3] SKIP.
               
               IF v-comp-addr3 NE "" OR v-ship-cityline NE "" THEN
                    PUT "<#3><C4>" v-comp-addr3     "<C46>" v-ship-cityline SKIP.
               
     PUT        "<C1><#3>"
                "<||5><=3><C1><#4><FROM><=4><R+4><C80><RECT>" SKIP
                "<=4><R+2><C1><FROM><=4><R+2><C80><LINE>" SKIP    
                "<=4><C9><FROM><=4><R+4><C9><LINE>" SKIP
                "<=4><C18><FROM><=4><R+4><C18><LINE>" SKIP      
                "<=4><C36><FROM><=4><R+4><C36><LINE>" SKIP                
                "<=4><C42><FROM><=4><R+4><C42><LINE>" SKIP
                "<=4><C48><FROM><=4><R+4><C48><LINE>" SKIP
                "<=4><C57><FROM><=4><R+4><C57><LINE>" SKIP
                /*"<R15><C70><FROM><R19><C70><LINE>" SKIP*/
                /*"<=4><C54>NMFC#"*/
                "<=4><R+0.1>                                                                      Freight                         " SKIP
                "<=4><R+1>    Date     FOB                   Carrier         NMFC#    Class     Terms             Weight             " SKIP
                /*"<=4><R+2><C54>29280"*/
                "<=4><R+3> " oe-bolh.bol-date "<C9.5>" v-fob "<C18.5>" carrier.dscr FORM "x(24)" "<C36.5>" "152465" "<C43.5>" "55" "<C48.5>" v-frt-terms "<C60>" v-tot-wt  SKIP
                "<||5><=4><R+5><C1><#5><FROM><=5><R+2><C80><RECT>" SKIP    
                "<=5><C12><FROM><=5><R+2><C12><LINE>" SKIP
                "<=5><C23><FROM><=5><R+2><C23><LINE>" SKIP
                "<=5><C42><FROM><=5><R+2><C42><LINE>" SKIP               
                "<=5><C58><FROM><=5><R+2><C58><LINE>" SKIP 
                "<=5><C63.5><FROM><=5><R+2><C63.5><LINE>"  SKIP
                "<=5><C75><FROM><=5><R+2><C75><LINE>" SKIP            
                
            "<=5><R+1> Part# / Job#            PO#          Description               Lot#               PAL      Unit-Quantity  Total" SKIP(1)
            .
            v-printline = v-printline + 20.

