/* ------------------------------------------ oe/rep/bolallws2.i GDM 04200904*/
/* BOL PRINT for N-K-1-BOLFMT = Allwest                                      */
/* ------------------------------------------------------------------------- */
 put "<FArial>"  SKIP
     "<P14><C+40><B>Bill Of Lading</B> " SKIP
     "<C1><#1><R+5><C+25>" SKIP
     "<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
     "<=1><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
     "<P10></B>"

     "<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
     "<P10><=1><R+3>"
     v-comp-add1 AT 8 SKIP.

IF TRIM(v-comp-add2) NE "" 
  THEN
   PUT
     v-comp-add2 AT 8 SKIP.

IF TRIM(v-comp-add3) NE "" 
  THEN
   PUT
     v-comp-add3 AT 8 SKIP.

IF TRIM(v-comp-add4) NE "" 
  THEN
   PUT
     v-comp-add4 AT 8 SKIP.

IF TRIM(v-comp-add5) NE "" 
  THEN
   PUT
     v-comp-add5 AT 8 SKIP.
     
IF TRIM(lv-email) NE "" 
  THEN
   PUT
     lv-email AT 8 .
PUT
   "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP(1).

PUT "<FCourier New>"
    "Sold To:" SPACE(30) "Ship To:"  SKIP.

IF TRIM(v-comp-name) NE "" OR
   TRIM(v-ship-name) NE ""
  THEN
   PUT SPACE(5) v-comp-name v-ship-name AT 45 SKIP.

IF TRIM(v-comp-addr[1]) NE "" OR
   TRIM(v-ship-addr[1]) NE "" 
  THEN
   PUT SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP.

IF TRIM(v-comp-addr[2]) NE "" OR
   TRIM(v-ship-addr[2]) NE "" 
  THEN
   PUT SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP.

IF TRIM(v-comp-addr3) NE "" OR
   TRIM(v-ship-addr3) NE "" 
  THEN
   PUT SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP.

ASSIGN v-frt-terms = IF oe-bolh.frt-pay EQ "P" 
                       THEN "Prepaid"
                       ELSE IF oe-bolh.frt-pay EQ "B" 
                              THEN "Bill"
                              ELSE IF oe-bolh.frt-pay EQ "C" 
                                     THEN "Collect"
                                     ELSE IF oe-bolh.frt-pay eq "T" 
                                            THEN "Third Party" else "".

PUT
     "<R5><C50><#3>" SKIP
     "<FArial><P14><=#3><P10>" SKIP
     "<=#3><B><P14>BOL #: " oe-bolh.bol-no "</B><P10>" SKIP(1)
     "<=#3><R+2>Date: " oe-bolh.bol-date        SKIP
     "<=#3><R+3>Contact: " v-shipto-contact SKIP
     "<=#3><R+4>Phone: " v-phone FORM "x(15)"  SKIP     
     "<|10><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
     "<R21><C1><FROM><R21><C81><LINE>" SKIP    
     "<R19><C12><FROM><R23><C12><LINE>" SKIP
     "<R19><C25><FROM><R23><C25><LINE>" SKIP
     "<R19><C40><FROM><R23><C40><LINE>" SKIP
     "<R19><C66><FROM><R23><C66><LINE>" SKIP
     "<FArial><=4><R+1>    Date                      FOB                                   Phone                                 Carrier                                          Freight Terms" SKIP 
     "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) v-fob space(5) v-ship-phone space(4) carrier.dscr SPACE(5)v-frt-terms SKIP
     "<|10><R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
     "<R24><C13><FROM><R26><C13><LINE>" SKIP
     "<R24><C26><FROM><R26><C26><LINE>" SKIP
     "<R24><C39><FROM><R26><C39><LINE>" SKIP
     "<R24><C56><FROM><R26><C56><LINE>" SKIP  
     "<R24><C65><FROM><R26><C65><LINE>" SKIP
     "<R24><C76><FROM><R26><C76><LINE>" SKIP.

 PUT "<FArial><=5><R+1> Part#/Order#                  PO# / Job#             Finished Good#              Description                 Unit-Quantity  Partial/Complete  Weight  <FCourier New>" SKIP(1).
 
 v-printline = v-printline + 16.
