 /* oe/rep/boltrilx2.i for Trilakes  */  
 
   put 
      "<FArial>"  SKIP
      "<#1><P10><ADJUST=LPI><C50><B>STRAIGHT BILL OF LADING"  SKIP
      "<=1><R+1><C25></B>"            SKIP
      "<=1><R+2><C25><B><C50>Bill of Lading #: " oe-bolh.bol-no   "</B>".

   IF NOT v-broker THEN
      PUT "<R-4><C1><#2><R+10><C+60><IMAGE#1=" ls-full-img1 SKIP.
   ELSE
      PUT "<=1><C3><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
          "<P10></B>"
          "<=1><R+1>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
         "<P10><=1><R+2>"  
         v-comp-add1 AT 8 SKIP
         v-comp-add2 AT 8  SKIP
         v-comp-add3 AT 8 SKIP
         v-comp-add4 AT 8 SKIP
         v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
         lv-email AT 8 SKIP(1).

   PUT "<FCourier New>"
      "Ship To:" SPACE(51) "<FArial><P14><B>" v-frt-class FORMAT "X(15)" "</B>" SKIP
      "<FCourier New><P10>"
      SPACE(5) v-ship-name skip
      SPACE(5) v-ship-addr[1] FORM "X(30)" SPACE(24) "<FArial><P14><B>" v-frt-desc FORMAT "X(30)" "</B>" SKIP
      "<FCourier New><P10>"
      SPACE(5) v-ship-addr[2] SKIP
      SPACE(5) v-ship-addr3 SKIP 
      "<R5><C50><#3>" SKIP
      "<FArial><P14><=#3><P10><ADJUST=LPI>" SKIP
      "<=#3>Customer Dock time_____________________" SKIP
       SKIP              
      "<=#3><R+2>Page #:             " PAGE-NUM SKIP
      "<=#3><R+3>Date  :                     " oe-bolh.bol-date        SKIP
      SKIP
      SKIP(3).

   PUT "<P10><ADJUST=LPI>"
       "<|10><R17><C1><#4><FROM><R21><C81><RECT>" SKIP
       "<R19><C1><FROM><R19><C81><LINE>" SKIP    
       "<R17><C12><FROM><R21><C12><LINE>" SKIP
       "<R17><C30><FROM><R21><C30><LINE>" SKIP 
       "<R17><C46><FROM><R21><C46><LINE>" SKIP
       "<R17><C66><FROM><R21><C66><LINE>" SKIP
       "<FArial><=4><R+1><P10><ADJUST=LPI>          Date                              FOB                                Trailer#                                 Carrier                                   Freight Terms" SKIP 
       "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) v-fob space(10) v-trailer SPACE(12) carrier.dscr FORM "x(25)" v-frt-terms SKIP
       "<|10><R22><C1><#5><FROM><R24><C81><RECT>" SKIP    
       "<R22><C13><FROM><R24><C13><LINE>" SKIP
       "<R22><C28><FROM><R24><C28><LINE>" SKIP
       "<R22><C37><FROM><R24><C37><LINE>" SKIP
       "<R22><C56><FROM><R24><C56><LINE>" SKIP  
       "<R22><C69><FROM><R24><C69><LINE>" SKIP
       "<R22><C73><FROM><R24><C73><LINE>" SKIP
       "<FArial><=5><C18>Order#/ "
       "<FArial><=5><R+1>    Item# / Part                Customer " IF lv-print-lot THEN "Lot# " ELSE "PO#" "           JOB#                       Description                     Unit-Quantity           P/C       Weight  " SKIP(1)
        "<FCourier New>".

   v-printline = v-printline + 16.
