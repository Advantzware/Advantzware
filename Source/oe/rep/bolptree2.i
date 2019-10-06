 /* oe/rep/bolptree2.i  */  
 IF vll-is-transfer THEN 
    vlc-transfer  = "TRANSFER".
 ELSE
    vlc-transfer = "".

 PUT "<FArial>"  SKIP
     "<P18>" 
     "<C+35><B>" vlc-transfer "</B>" "<C60><P14><B>" "Bill Of Lading"  "</B>" .
     if v-pg-num EQ 1 THEN   
     put  "<UNITS=INCHES><AT=.60,6><FROM><AT=+.5,+2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" +
                    "*" + string(oe-bolh.bol-no) + "*" + ">" FORM "x(100)" "</B>"
      "<=1><R+1><C25></B>"            SKIP .

    put "<P10>" SKIP.
 PUT "<C2><R4><#1><R+8><C40><IMAGE#1=" ls-full-img1 SKIP.
     
 PUT "<FCourier New>"
     "Sold To: " v-phone SPACE(37) "Ship To: "  v-ship-phone SKIP
     SPACE(5) v-comp-name v-ship-name AT 65 skip
     SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 65 SKIP
     SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 65 SKIP
     SPACE(5) v-comp-addr3 v-ship-addr3 AT 65 SKIP
    "<R8><C50><#3>" SKIP
    "<FArial><P14><=#3><P10>" SKIP(2)
    "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SPACE(7).

 PUT UNFORMATTED
    "Page #:              " + STRING(v-pg-num) + " of " + v-str SKIP.

 PUT
    "<=#3><R+2>Date: " oe-bolh.bol-date        SKIP
    "<=#3><R+3>" SKIP
     SKIP     
    "<|10><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
    "<R21><C1><FROM><R21><C81><LINE>" SKIP    
    "<R19><C12><FROM><R23><C12><LINE>" SKIP
    "<R19><C45><FROM><R23><C45><LINE>" SKIP
    "<R19><C66><FROM><R23><C66><LINE>" SKIP
    "<FArial><=4><R+1>    Date                    FOB                                                                                   " v-carrier "                             Freight Terms" SKIP 
    "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) v-fob space(31) oe-bolh.trailer space(5) v-frt-terms SKIP
    "<|10><R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
    "<R24><C13><FROM><R26><C13><LINE>" SKIP
    "<R24><C26><FROM><R26><C26><LINE>" SKIP
    "<R24><C39><FROM><R26><C39><LINE>" SKIP
    "<R24><C56><FROM><R26><C56><LINE>" SKIP  
    "<R24><C65><FROM><R26><C65><LINE>" SKIP
     SKIP            
    "<FArial><=5><R+1> Part#                        PO#                            Finished Good#        Description                         Unit-Quantity              P /  C        " SKIP(1)
    "<FCourier New>".

 v-printline = v-printline + 16.
