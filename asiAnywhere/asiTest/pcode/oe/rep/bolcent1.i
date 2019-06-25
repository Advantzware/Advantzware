/* oe/rep/bolcent1.i */      
      put 
         "<FMS Mincho>"
          "<P14><R1><C46><B>Bill of lading #:" OE-BOLH.BOL-NO "</B><P10> "
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
              "<R5><C1>"
               space(7) "Sold To:" SPACE(49) "Ship To:"  SKIP(1)
               SPACE(7) v-comp-name v-ship-name AT 65 skip
               SPACE(7) v-comp-addr[1] v-ship-addr[1] AT 65 SKIP
               SPACE(7) v-comp-addr[2] v-ship-addr[2] AT 65 SKIP
               SPACE(7) v-comp-addr3 v-ship-addr3 AT 65 SKIP
        "<R2><C46><#3>" SKIP
     /* "<P14><=#3>" /*<C-20><R-2> <B>Bill Of Lading</B>*/ "<P10>" SKIP
                "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP */
                "<=#3><R+1>Ship Date:" oe-bolh.bol-date        SKIP
             /*   "<=#3><R+2>" /*Ship Date:" oe-bolh.ship-date        */ SKIP
                 SKIP                                                      */
                "<||5><R12><C1><#4><FROM><R16><C80><RECT>" SKIP
                "<R14><C1><FROM><R14><C80><LINE>" SKIP    
                "<R12><C12><FROM><R16><C12><LINE>" SKIP
                "<R12><C25><FROM><R16><C25><LINE>" SKIP      
                "<R12><C34><FROM><R16><C34><LINE>" SKIP                
                "<R12><C53.3><FROM><R16><C53.3><LINE>" SKIP
                "<R12><C58.5><FROM><R16><C58.5><LINE>" 
                "<R12><C69><FROM><R16><C69><LINE>" SKIP
                "<=4><C54>NMFC#"
                "<=4><R+1>    Date              JOB#             FOB             Carrier              Class  Freight Terms         Weight" SKIP
                "<=4><R+2><C54>152465"
                "<=4><R+3> " oe-bolh.bol-date SPACE(3) /*v-po-no FORM "x(15)" */ space(6) v-job-no space(6) v-fob space(1) carrier.dscr FORM "x(28)" space(1) "55" space(4) v-frt-terms space(7) v-tot-wt SKIP
                "<||5><R17><C1><#5><FROM><R19><C80><RECT>" SKIP    
                "<R17><C12><FROM><R19><C12><LINE>" SKIP
                "<R17><C23><FROM><R19><C23><LINE>" SKIP
                "<R17><C42><FROM><R19><C42><LINE>"                
                "<R17><C58><FROM><R19><C58><LINE>" SKIP 
                "<R17><C64><FROM><R19><C64><LINE>" 
                "<R17><C75.2><FROM><R19><C75.2><LINE>" SKIP            
                
            "<=5><R+1>    Part#             PO#                Description              Tag#               PAL    Unit-Quantity  Total" SKIP(1)
            .
            v-printline = v-printline + 16.
