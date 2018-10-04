 /* oe/rep/bolcapcin2.i  */  
 
 put "<FArial>"  SKIP
     "<C3><R2><#1><R+12><C50><IMAGE#1=" ls-full-img1 
     "<P15><R2><C47><B>Bill Of Lading</B><FCourier New><P10>" SKIP
     "<R12><C1>"
     "Sold To:" SPACE(46) "Ship To:"  SKIP
     SPACE(5) v-comp-name v-ship-name AT 60 skip
     SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 60 SKIP
     SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 60 SKIP
     SPACE(5) v-comp-addr3 v-ship-addr3 AT 60 SKIP
     "<R5><C50><#3>" SKIP
     "<FArial><P10><=#3><P10>" SKIP
     "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP(1)
     "<=#3><R+2>Date: " oe-bolh.bol-date        SKIP
     "<=#3><R+3>Contact: " v-shipto-contact SKIP
     "<=#3><R+4>Phone: " v-phone FORM "x(15)"  SKIP     
     "<|10><R17><C1><#4><FROM><R21><C78><RECT>" SKIP
     "<R19><C1><FROM><R19><C78><LINE>" SKIP    
     "<R17><C12><FROM><R21><C12><LINE>" SKIP
     "<R17><C30><FROM><R21><C30><LINE>" SKIP
     "<R17><C43><FROM><R21><C43><LINE>" SKIP
     "<R17><C66><FROM><R21><C66><LINE>" SKIP
     "<FArial><=4><R+1>    Date                    FOB                                        Phone                             Carrier                                          Freight Terms" SKIP 
     "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) v-fob space(10) v-ship-phone space(3) carrier.dscr v-frt-terms SKIP
     "<|10><R22><C1><#5><FROM><R24><C78><RECT>" SKIP    
     "<R22><C14><FROM><R24><C14><LINE>" SKIP
     "<R22><C27><FROM><R24><C27><LINE>" SKIP
     /*"<R22><C36><FROM><R24><C36><LINE>" SKIP*/
     "<R22><C57><FROM><R24><C57><LINE>" SKIP  
     "<R22><C62><FROM><R24><C62><LINE>" SKIP
     "<R22><C70><FROM><R24><C70><LINE>" SKIP
     "<R22><C73><FROM><R24><C73><LINE>" SKIP.

 PUT "<FArial><=5> Order Qty                  Customer PO# <FCourier New>" SKIP.
 PUT "<FArial><=5><R+1> Customer Part#         Axis Order#                                      Description                                     Unit/s    Quantity       P/C  Weight  <FCourier New>" SKIP(1).
 
 v-printline = v-printline + 16.
