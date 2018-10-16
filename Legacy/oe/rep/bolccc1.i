/* oe/rep/bolccc1.i */      

     IF NOT lv-print-img THEN
       put 
         "<FMS Mincho>"
          "<P14><R4><C52><B>Bill of lading #:" OE-BOLH.BOL-NO "</B><P10> "
          "<C3><R2><#1><C+3><R+8><C+45><IMAGE#1=" ls-full-img1  SKIP    .
      IF lv-print-img THEN
          PUT  "<FMS Mincho>"
          "<P14><R3><C52><B>Bill of lading #:" OE-BOLH.BOL-NO "</B><P10> " SKIP
          "<R3><C5>Manufactured by:"  "<R3><C30>Distributed by:"  SKIP
          "<C3><R4><#1><C+3><R+6><C+45><IMAGE#1=" ls-full-img1
           "<C30><R4><#1><C+3><R+6><C+45><IMAGE#1=" ls-full-img2 SKIP .

      PUT
              "<R10><C1>" 
               space(4) "Sold To:" SPACE(49) "Ship To:"  SKIP
               SPACE(4) v-comp-name v-ship-name AT 65 skip
               SPACE(4) v-comp-addr[1] v-ship-addr[1] AT 65 SKIP
               SPACE(4) v-comp-addr[2] v-ship-addr[2] AT 65 SKIP
               SPACE(4) v-comp-addr3 v-ship-addr3 AT 65 SKIP
        "<R3><C46><#3>" SKIP

                "<||5><R15><C1><#4><FROM><R19><C80><RECT>" SKIP
                "<R17><C1><FROM><R17><C80><LINE>" SKIP    
                "<R15><C9><FROM><R19><C9><LINE>" SKIP
                "<R15><C18><FROM><R19><C18><LINE>" SKIP      
                "<R15><C36><FROM><R19><C36><LINE>" SKIP                
                "<R15><C42><FROM><R19><C42><LINE>" SKIP
                "<R15><C48><FROM><R19><C48><LINE>" 
                "<R15><C57><FROM><R19><C57><LINE>" SKIP
                "<R15><C70><FROM><R19><C70><LINE>" SKIP
                /*"<=4><C54>NMFC#"*/
                "<=4><R+0.1>                                                                      Freight                         Requested" SKIP
                "<=4><R+1>    Date               FOB                   Carrier                            NMFC#       Class             Terms               Weight               Delivery" SKIP
                /*"<=4><R+2><C54>29280"*/
                "<=4><R+3> " oe-bolh.bol-date "<C9.5>" v-fob "<C18.5>" carrier.dscr FORM "x(24)" "<C36.5>" "152465" "<C43.5>" "55" "<C48.5>" v-frt-terms "<C60>" v-tot-wt "<C72>" dReqDate SKIP
                "<||5><R20><C1><#5><FROM><R22><C80><RECT>" SKIP    
                "<R20><C12><FROM><R22><C12><LINE>" SKIP
                "<R20><C23><FROM><R22><C23><LINE>" SKIP
                "<R20><C42><FROM><R22><C42><LINE>"                
                "<R20><C58><FROM><R22><C58><LINE>" SKIP 
                "<R20><C64><FROM><R22><C64><LINE>" 
                "<R20><C75.2><FROM><R22><C75.2><LINE>" SKIP            
                
            "<=5><R+1> Part# / Job#            PO#                          Description                               Lot#                                  PAL         Unit-Quantity     Total" SKIP(1)
            .
            v-printline = v-printline + 16.
