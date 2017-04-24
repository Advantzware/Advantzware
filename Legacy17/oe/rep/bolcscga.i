/* oe/rep/bolcscga.i  */  
 
PUT
  "<FArial>"  SKIP
  "<P14><C+40><B>Bill Of Lading</B>     Page: " STRING(PAGE-NUM - lv-pg-num,">>9") /*+ " of " + STRING(lv-tot-pg) */FORM "x(20)" SKIP
  "<C1><R-1><#1><R+5><C+25>" "<IMAGE#1=" + ls-full-img1 FORM "x(60)" SKIP 
  "<=1><R+2>" "<FGCOLOR=" + TRIM(lv-comp-color) + ">" FORM "x(15)"
  "<P10><=1><R+4>"
  v-comp-add1 AT 8 SKIP
  v-comp-add2 AT 8 SKIP
  v-comp-add3 AT 8 SKIP
  v-comp-add4 AT 8 SKIP
  v-comp-add5 AT 8 "<FGCOLOR=" + TRIM(lv-other-color) + ">" FORM "x(15)" SKIP(2)
  "<FCourier New>"
  SPACE(30) "Ship To:" AT 59  SKIP
  SPACE(5) v-comp-name v-ship-name AT 45 skip
  SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
  SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
  SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP
  "<R5><C50><#3>" SKIP
  "<FArial><P14><=#3><P10>" SKIP
  "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP(1)
  "<=#3><R+2>Date: " string(TODAY) /*oe-bolh.bol-date*/ SKIP
  "<=#3><R+3>" SKIP
  SKIP     
  "<|10><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
  "<R21><C1><FROM><R21><C81><LINE>" SKIP    
  "<R19><C12><FROM><R23><C12><LINE>" SKIP
  "<R19><C30><FROM><R23><C30><LINE>" SKIP
  "<R19><C49><FROM><R23><C49><LINE>" SKIP
  "<R19><C66><FROM><R23><C66><LINE>" SKIP
  "<FArial><=4><R+1>    Due Date                     FOB                                        Trailer#                                                                            Freight Terms" SKIP 
  "<FCourier New><=4><R+3> " vDate SPACE(5) v-fob space(10) carrier.dscr FORM "x(22)" oe-bolh.trailer FORM "x(21)" v-frt-terms SKIP
  "<|10><R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
  "<R24><C13><FROM><R26><C13><LINE>" SKIP
  "<R24><C26><FROM><R26><C26><LINE>" SKIP
  "<R24><C56><FROM><R26><C56><LINE>" SKIP  
  "<R24><C61><FROM><R26><C61><LINE>" SKIP
  "<R24><C67><FROM><R26><C67><LINE>" SKIP            
  "<R24><C74><FROM><R26><C74><LINE>" SKIP 
  "<FArial><=5><R+1>  Order Qty / FG#           PO# / Order#                      Description                                              Units    Count         Total          P/C" SKIP(1)
  "<FCourier New>"                                  
  .
  v-printline = v-printline + 17.
