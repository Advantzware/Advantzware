 /* oe/rep/bolprmpx2.i for PremierPX  */  
 
   

   put 
         "<FArial>"  SKIP
         "<#1><P10><ADJUST=LPI><C50><B>STRAIGHT BILL OF LADING"  SKIP
         "<=1><R+1><C25></B>"            SKIP
         "<=1><R+2><C25><B><C50>Bill of Lading #: " oe-bolh.bol-no   "</B>"
         "<R-4><C1><#2><R+10><C+60><IMAGE#1=" ls-full-img1 SKIP
               "<FCourier New>"
               "Bill To:" SPACE(30) "Ship To:"  SKIP
               SPACE(5) v-comp-name v-ship-name AT 45 skip
               SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
             SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
               SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP 
        "<R5><C50><#3>" SKIP
        "<FArial><P14><=#3>" "<P10><ADJUST=LPI>" SKIP
                "<=#3>Customer Dock time_____________________" SKIP
                 SKIP              
                "<=#3><R+2>Page #:             " PAGE-NUM SKIP
                "<=#3><R+3>Date  :                     " oe-bolh.bol-date        SKIP
                SKIP
                SKIP(3).

       PUT    "<P10><ADJUST=LPI>"
                "<|5><R17><C1><#4><FROM><R21><C81><RECT>" SKIP
                "<R19><C1><FROM><R19><C81><LINE>" SKIP    
                "<R17><C12><FROM><R21><C12><LINE>" SKIP
                "<R17><C30><FROM><R21><C30><LINE>" SKIP 
                "<R17><C46><FROM><R21><C46><LINE>" SKIP
                "<R17><C66><FROM><R21><C66><LINE>" SKIP
                "<FArial><=4><R+1><P10><ADJUST=LPI>          Date                              FOB                                Trailer#                                 Carrier                                   Freight Terms" SKIP 
                "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) v-fob space(10) v-trailer SPACE(12) carrier.dscr v-frt-terms SKIP
                "<|5><R22><C1><#5><FROM><R24><C81><RECT>" SKIP    
                "<R22><C13><FROM><R24><C13><LINE>" SKIP
                "<R22><C28><FROM><R24><C28><LINE>" SKIP
                "<R22><C58><FROM><R24><C58><LINE>" SKIP
                "<R22><C64><FROM><R24><C64><LINE>" SKIP
                "<R22><C71><FROM><R24><C71><LINE>" SKIP
                "<FArial><=5><C5>Item#                         PO#                                 Item Name/Customer Part#                                          Per"
                "<FArial><=5><R+1><C5>Order#                       JOB#                               Product Description                                     Unit           Unit             Total         " SKIP(1)
            "<FCourier New>"                                  
            .

            v-printline = v-printline + 16.
