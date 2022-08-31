/* --------------------------------------------- oe/rep/bolloyln1.i 10/09 GDM */
/* N-K BOLFMT = Loylang - FORM for Loylang                                    */
/* -------------------------------------------------------------------------- */ 
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No). */
IF NOT v-broker
  THEN PUT "<FArial>"  SKIP
           "<P14><C+45><B>Bill Of Lading</B> "
           "<C1><R1><#1><C+3><R+8><C+45><IMAGE#1=" ls-full-img1 SKIP(2)
           "<FCourier New><P10>"
           "Sold To:" SPACE(30) "Ship To:"  SKIP
           SPACE(5) v-comp-name    v-ship-name    AT 45 SKIP
           SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
           SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
           SPACE(5) v-comp-addr3   v-ship-addr3   AT 45 SKIP
           .

  ELSE PUT "<FArial><P14>"  SKIP
           "<C+45><B>Bill Of Lading</B>"
           "<C1><R4><#1><C+4>"             
           "<=#1><R+1><C+6>" v-comp-name    
           "<=#1><R+2><C+6>" v-comp-addr[1] 
           "<=#1><R+3><C+6>" v-comp-addr[2] 
           "<=#1><R+4><C+6>" v-comp-addr3   
           "<FCourier New><P10><C1><R+5>"                            
           SPACE(38) "Ship To:"  SKIP            
           v-ship-name    AT 45 SKIP
           v-ship-addr[1] AT 45 SKIP
           v-ship-addr[2] AT 45 SKIP
           v-ship-addr3   AT 45 SKIP
           .

PUT  "<R5><C50><#3>" SKIP
     "<FArial><P14><=#3><P10>" SKIP
     "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP(1)
     "<=#3><R+2>Date: " oe-bolh.bol-date        SKIP
     "<=#3><R+3>Contact: " v-shipto-contact SKIP
     "<=#3><R+4>Phone: " v-phone FORM "x(15)"  SKIP.

PUT  "<|10><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
     "<R21><C1><FROM><R21><C81><LINE>" SKIP    
     "<R19><C11><FROM><R23><C11><LINE>" SKIP
     "<R19><C22><FROM><R23><C22><LINE>" SKIP
     "<R19><C46><FROM><R23><C46><LINE>" SKIP
     "<R19><C66><FROM><R23><C66><LINE>" SKIP
     "<FArial><=4><R+1>    Date                    FOB                                        Phone                                 Carrier                                          Freight Terms" SKIP 
     "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(2) v-fob space(10) v-ship-phone space(7) carrier.dscr v-frt-terms SKIP
     "<|10><R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
     "<R24><C11.5><FROM><R26><C11.5><LINE>" SKIP
     "<R24><C24.5><FROM><R26><C24.5><LINE>" SKIP
     "<R24><C37><FROM><R26><C37><LINE>" SKIP
     "<R24><C61><FROM><R26><C61><LINE>" SKIP  
     "<R24><C73><FROM><R26><C73><LINE>" SKIP
     "<R24><C76><FROM><R26><C76><LINE>" SKIP.
 /*PUT "<FArial><=5><R+1> Order#       PO# / Job#             FG # / Cust Part            Item Name                                                  Unit-Quantity            P/C  Weight  <FCourier New>" SKIP(1). */
 PUT "<FArial><=5><R+1>  Order#/Job#
                   <C13> PO# / Lot#
                   <C25> FG # / Cust Part
                   <C38> Item Name
                   <C62> Unit-Quantity
                   <C73> P/C
                   <C76> Weight  <FCourier New>" SKIP(1).
                                                                       
 v-printline = v-printline + 16.
