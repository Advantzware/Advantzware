/* oe/rep/bolcent1.i */      
      put 
         "<FMS Mincho>"
          "<P14><R2><C46><B>Bill of lading #:" OE-BOLH.BOL-NO "</B><P10> "
         /* "<C3><R2><#1><C+3><R+8><C+45><IMAGE#1=" ls-full-img1 */
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
              "<R6><C1>"
               space(7) "Sold To:" SPACE(49) "Ship To:"  SKIP(1)
               SPACE(7) v-comp-name v-ship-name AT 65 skip
               SPACE(7) v-comp-addr[1] v-ship-addr[1] AT 65 SKIP
               SPACE(7) v-comp-addr[2] v-ship-addr[2] AT 65 SKIP
               SPACE(7) v-comp-addr3 v-ship-addr3 AT 65 SKIP
        "<R3><C46><#3>" SKIP
     /* "<P14><=#3>" /*<C-20><R-2> <B>Bill Of Lading</B>*/ "<P10>" SKIP
                "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP */
                "<=#3><R+1>Ship Date:" oe-bolh.bol-date        SKIP
             /*   "<=#3><R+2>" /*Ship Date:" oe-bolh.ship-date        */ SKIP
                 SKIP                                                      */
                "<||5><R13><C1><#4><FROM><R17><C80><RECT>" SKIP
                "<R15><C1><FROM><R15><C80><LINE>" SKIP    
                "<R13><C9><FROM><R17><C9><LINE>" SKIP
                "<R13><C18><FROM><R17><C18><LINE>" SKIP      
                "<R13><C36><FROM><R17><C36><LINE>" SKIP  
                "<R13><C42><FROM><R17><C42><LINE>" SKIP
                "<R13><C48><FROM><R17><C48><LINE>" 
                "<R13><C57><FROM><R17><C57><LINE>" SKIP
                "<R13><C70><FROM><R17><C70><LINE>" SKIP
                /*"<=4><C54>NMFC#"*/
                "<=4><R+0.1>                                                                      Freight                         Requested" SKIP
                "<=4><R+1>    Date        FOB              Carrier           NMFC#    Class     Terms            Weight         Delivery" SKIP
                /*"<=4><R+2><C54>29280"*/
                "<=4><R+3> " oe-bolh.bol-date "<C9.5>" v-fob "<C18.5>" carrier.dscr FORM "x(24)" "<C36.5>" "152465" "<C43.5>" "55" "<C48.5>" v-frt-terms "<C60>" v-tot-wt "<C72>" dReqDate SKIP
                "<||5><R18><C1><#5><FROM><R20><C80><RECT>" SKIP    
                "<R18><C12><FROM><R20><C12><LINE>" SKIP
                "<R18><C23><FROM><R20><C23><LINE>" SKIP
                "<R18><C42><FROM><R20><C42><LINE>"                
                "<R18><C58><FROM><R20><C58><LINE>" SKIP 
                "<R18><C64><FROM><R20><C64><LINE>" 
                "<R18><C75.2><FROM><R20><C75.2><LINE>" SKIP            
                
            "<=5><R+1> Part# / Job#         PO#                Description              Tag#               PAL    Unit-Quantity  Total" SKIP(1)
            .
            v-printline = v-printline + 16.
