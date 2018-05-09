 /* oe/rep/bolptreebc2c.i */  

   IF vll-is-transfer THEN 
      vlc-transfer  = "TRANSFER ".
   ELSE
      vlc-transfer = "".

   put 
      "<FArial>"  SKIP
      "<#1><P10><ADJUST=LPI><C+35><B><P18>" vlc-transfer  "</B><P10>"
      "<C50><B>STRAIGHT BILL OF LADING"  SKIP
      "<=1><R+1><C25></B>"            SKIP
      "<=1><R+5.5><C25><B><C50>Bill of Lading #: " oe-bolh.bol-no   "</B>".
   IF v-pg-num = 1 THEN
    PUT  
     "<UNITS=INCHES><AT=.62,6><FROM><AT=+.4,+2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" +
                    string(oe-bolh.bol-no) + "*" + ">" FORM "x(100)" "</B><P10>"
       
       "<C1><R2><FROM><R12><C15><#89><AT=0.01,0.01>"
      .   

   IF (AVAIL shipto AND shipto.broker EQ NO) OR NOT AVAIL shipto THEN
      PUT "<IMAGE#89=" ls-full-img1 SKIP
          "<=1><C15><FGCOLOR=" trim(lv-comp-color) + ">"
          "<=1><C15><R+2><P16><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)"
          "<P10></B>"
          "<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
          "<P10><=1><R+4>"
          "<C15>" v-comp-add1 SKIP
          "<C15>" v-comp-add2 SKIP
          "<C15>" v-comp-add3 SKIP
          "<C15>" v-comp-add4 SKIP
          "<C15>" v-comp-add5 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
          "<C15>" lv-email SKIP.
   ELSE
      PUT SKIP
          "<=1><C1><FGCOLOR=" trim(lv-comp-color) + ">"
          "<=1><C1><R+2><P16><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)"
          "<P10></B>"
          "<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
          "<P10><=1><R+4>"
          "<C1>" v-comp-add1 SKIP
          "<C1>" v-comp-add2 SKIP
          "<C1>" v-comp-add3 SKIP
          "<C1>" v-comp-add4 SKIP
          "<C1>" v-comp-add5 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
          "<C1>" lv-email SKIP.

   PUT "<FCourier New>" SKIP(1)
      "Bill To: " v-phone SPACE(35) "Ship To: " v-ship-phone SKIP
      SPACE(5) v-comp-name v-ship-name AT 65 skip
      SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 65 SKIP
      SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 65 SKIP
      SPACE(5) v-comp-addr3 v-ship-addr3 AT 65 SKIP 
      "<R8.5><C50><#3>" SKIP
      "<FArial><P14><=#3>" "<P10><ADJUST=LPI>" SKIP
      "<=#3>Customer Dock time_____________________" SKIP .
      

   PUT UNFORMATTED
      "<=#3><R+2>Page #:              " + STRING(v-pg-num) + " of " + v-str SKIP.

   PUT "<=#3><R+3>Date  :                     " oe-bolh.bol-date        SKIP
      SKIP 
      SKIP(3).

   PUT "<P10><ADJUST=LPI>"
       "<|10><R20><C1><#4><FROM><R24><C81><RECT>" SKIP
       "<R22><C1><FROM><R22><C81><LINE>" SKIP    
       "<R20><C12><FROM><R24><C12><LINE>" SKIP
       "<R20><C29><FROM><R24><C29><LINE>" SKIP 
       "<R20><C47><FROM><R24><C47><LINE>" SKIP
       "<R20><C66><FROM><R24><C66><LINE>" SKIP
       "<FArial><=4><R+1><P10><ADJUST=LPI>          Date                              FOB                                Trailer#                                 Carrier                                   Freight Terms" SKIP 
       "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) v-fob space(9) oe-bolh.trailer FORMAT "X(20)" SPACE(1) carrier.dscr v-frt-terms SKIP
       "<|10><R25><C1><#5><FROM><R27><C81><RECT>" SKIP    
       "<R25><C13><FROM><R27><C13><LINE>" SKIP
       "<R25><C28><FROM><R27><C28><LINE>" SKIP
       "<R25><C56><FROM><R27><C56><LINE>" SKIP
       "<R25><C62><FROM><R27><C62><LINE>" SKIP
       "<R25><C69><FROM><R27><C69><LINE>" SKIP
       "<FArial><=5><C5>Item#                         PO#                                 Item Name/Customer Part#                                    Per"
       "<FArial><=5><R+1><C5>Order#                       JOB#                               Product Description                                 Unit         Unit                 Total         " SKIP(1)
       "<FCourier New>".

   v-printline = v-printline + 20.
