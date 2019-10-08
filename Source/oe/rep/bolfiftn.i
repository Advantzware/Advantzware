/* oe/rep/bolfibrex.i  */  
 
v-line1 = STRING(oe-bolh.bol-date,"99/99/9999") + FILL(" ",3) + STRING(v-fob,"X(11)")
        + "  " + STRING(oe-bolh.trailer,"X(20)") + "  " + STRING(carrier.dscr,"X(30)")
        + "  " + v-frt-terms.

PUT
  "<FArial>"  SKIP
  "<P14><C+50><B>Bill of Lading</B>" SKIP
  "<C1><R-1><#1><R+5><C+25>" .
  PUT  "<C2><R2><#1><R+8><C+48><IMAGE#1=" ls-full-img1  SKIP.


  PUT "<FGCOLOR=" + TRIM(lv-other-color) + ">" FORM "x(15)" SKIP
  SKIP(1)
  "<FCourier New>"
  SPACE(30) "Ship To:" AT 59  SKIP
  SPACE(5) v-comp-name v-ship-name AT 45 SKIP
  SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
  SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
  SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP
  "<R6><C51><#3>" SKIP
  "<FArial><P14><=#3><P10>" SKIP
  "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP(1)
  SKIP
  "<=#3><R+3>" SKIP
  SKIP     
  "<|10><R20><C1><#4><FROM><R24><C81><RECT>" SKIP
  "<R22><C1><FROM><R22><C81><LINE>" SKIP    
  "<R20><C12><FROM><R24><C12><LINE>" SKIP
  "<R20><C23><FROM><R24><C23><LINE>" SKIP
  "<R20><C41><FROM><R24><C41><LINE>" SKIP
  "<R20><C67><FROM><R24><C67><LINE>" SKIP
  "<FArial><=4><R+1>          Date                      FOB                            Trailer #                                            Carrier                                    Freight Terms" SKIP 
  "<FCourier New><=4><R+3> " v-line1 SKIP
  "<|10><R25><C1><#5><FROM><R27><C81><RECT>" SKIP    
  "<R25><C13><FROM><R27><C13><LINE>" SKIP
  "<R25><C26><FROM><R27><C26><LINE>" SKIP
  "<R25><C56><FROM><R27><C56><LINE>" SKIP  
  "<R25><C61><FROM><R27><C61><LINE>" SKIP
  "<R25><C67><FROM><R27><C67><LINE>" SKIP            
  "<R25><C77><FROM><R27><C77><LINE>" SKIP 
  "<FArial><=5><R+1> Order Qty / FG#             PO# / Job#                             Description / Lot#                               Units    Count                  Total       P/C" SKIP(1)
  "<FCourier New>".
  v-printline = v-printline + 17.
