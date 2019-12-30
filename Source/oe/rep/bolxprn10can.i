 /* oe/rep/bolxprn20.i  */  
 
 put "<FArial>"  SKIP
          "<P14><C+46><B>Connaissement / Bill of Lading</B> " SKIP
           "<C2><R2><#1><R+10><C+45><IMAGE#1=" ls-full-img1  SKIP
         
               "<FCourier New>"
               "Vendu À:" SPACE(23) "Livrè À:<P10>"   SKIP
               SPACE(5) v-comp-name v-ship-name AT 48 skip
               SPACE(5) v-comp-addr[1] FORMAT "x(40)" v-ship-addr[1] AT 48 FORMAT "x(40)" SKIP
             SPACE(5) v-comp-addr[2] FORMAT "x(40)" v-ship-addr[2]  AT 48 FORMAT "x(40)" SKIP
               SPACE(5) v-comp-addr3 FORMAT "x(40)" v-ship-addr3 AT 48 FORMAT "x(40)" SKIP
               SPACE(5) cCustContact  cShipContact  AT 48 SKIP
        "<R5><C50><#3>" SKIP
        "<FArial><P14><=#3>" /*<C-20><R-2> <B>Bill Of Lading</B> */ "<P10>" SKIP
                "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP(1)
                "<=#3><R+2>Date: " oe-bolh.bol-date        SKIP
                "<=#3><R+3>Contact: " v-shipto-contact SKIP
                "<=#3><R+4>Tèl: " v-phone FORM "x(15)"   SKIP     
                "<|10><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
                "<R21><C1><FROM><R21><C81><LINE>" SKIP    
                "<R19><C12><FROM><R23><C12><LINE>" SKIP
              /*  "<R19><C25><FROM><R23><C25><LINE>" SKIP      
                "<R19><C34><FROM><R23><C34><LINE>" SKIP */
                "<R19><C30><FROM><R23><C30><LINE>" SKIP
                "<R19><C46><FROM><R23><C46><LINE>" SKIP
                "<R19><C66><FROM><R23><C66><LINE>" SKIP
                /*"<FArial><=4><R+1>    Date                    PO#                               JOB#                 FOB                  Carrier                                                 Freight Terms" SKIP */
                "<FArial><=4><R+1>    Date                    Destination                                   Tel:                                 Transporteur/Carrier                 Conditions" SKIP 
                "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) /*v-po-no FORM "x(15)" space(2) v-job-no*/ v-fob space(10) v-ship-phone space(7) carrier.dscr FORMAT "x(21)" space(4) v-frt-terms SKIP
                "<|10><R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
                "<R24><C15><FROM><R26><C15><LINE>" SKIP
                "<R24><C26><FROM><R26><C26><LINE>" SKIP
                "<R24><C39><FROM><R26><C39><LINE>" SKIP
                "<R24><C58><FROM><R26><C58><LINE>" SKIP
                "<R24><C64><FROM><R26><C64><LINE>" SKIP
                "<R24><C71><FROM><R26><C71><LINE>" SKIP
                "<R24><C74><FROM><R26><C74><LINE>" SKIP.
   
   IF lv-bolfmt-int = 1 THEN PUT "<FArial><=5> Partie#/Commande#                                                                                                                    Unitès     Qtè/Unitès            Poids  <FCourier New>" SKIP(1) 
                                 "<FArial><=5><R+1> Part#/Order#                  PO#                    Finished Good#            Description                            Unit         Qty/Unit     P/C    Weight  <FCourier New>" SKIP(1).
                       ELSE  PUT
                                "<FArial><=5> Partie#                                  PO#               Finished Good#              Description                        Unitès     Qtè/Unitès  P/C     Poids  <FCourier New>" SKIP(1) 
                                "<FArial><=5><R+1> Partie#                                  PO#               Finished Good#              Description                         Unites    Qtè/Unitès  P/C     Weight  <FCourier New>" SKIP(1).
            .

            v-printline = v-printline + 16.
