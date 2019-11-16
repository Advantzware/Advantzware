 /* oe/rep/bolwinget20.i  */  
 
 
   put "<FArial>"  SKIP
          "<P14><C+49><B>Packing Slip</B> " SKIP .
          if NOT lBroker THEN DO:
           PUT  "<C2><R2><#1><R+9><C+38><IMAGE#1=" ls-full-img1  SKIP.
          END.
          ELSE DO:
           Put "<C1><#1><R+5><C+25>" 
		"<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
                "<=1><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
                "<P10></B>"

          	"<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
         	"<P10><=1><R+3>"  
         	v-comp-add1 AT 8 SKIP
         	v-comp-add2 AT 8  SKIP
         	v-comp-add3 AT 8 SKIP
         	v-comp-add4 AT 8 SKIP
         	v-comp-add5 AT 8 SKIP
         	lv-email AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP(1) .
	  END.
         
             Put  "<FArial><P10>"
               "<C5><B>Sold To:"  "<C50>Ship To:</B>"  SKIP(1)
               "<C5>" v-comp-name "<C48> "v-ship-name AT 45 skip
               "<C5>" v-comp-addr[1] "<C48>" v-ship-addr[1] AT 45 SKIP
               "<C5>" v-comp-addr[2] "<C48>" v-ship-addr[2] AT 45 SKIP
               "<C5>" v-comp-addr3 "<C48>" v-ship-addr3 AT 45 SKIP
        "<R5><C50><#3>" SKIP 
        "<FArial><P14><=#3>" /*<C-20><R-2> <B>Bill Of Lading</B> */ "<P10>" SKIP
                "<=#3><B>Packing Slip #: " oe-bolh.bol-no "</B>" SKIP(1)
                "<=#3><R+2>Date: " oe-bolh.bol-date        SKIP
                "<=#3><R+3>Contact: " v-shipto-contact SKIP
                "<=#3><R+4>Phone: " v-phone FORM "x(15)"  SKIP  
                 "<=#3><R+5>Order PO#: " cPoNo   SKIP 
                "<|10><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
                "<R21><C1><FROM><R21><C81><LINE>" SKIP    
                "<R19><C12><FROM><R23><C12><LINE>" SKIP
              /*  "<R19><C25><FROM><R23><C25><LINE>" SKIP      
                "<R19><C34><FROM><R23><C34><LINE>" SKIP */
                "<R19><C30><FROM><R23><C30><LINE>" SKIP
                "<R19><C46><FROM><R23><C46><LINE>" SKIP
                "<R19><C66><FROM><R23><C66><LINE>" SKIP
                /*"<FArial><=4><R+1>    Date                    PO#                               JOB#                 FOB                  Carrier                                                 Freight Terms" SKIP */
                "<FArial><=4><R+1>    Date                    FOB                                        Phone                                 Carrier                                          Freight Terms" SKIP 
                "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) /*v-po-no FORM "x(15)" space(2) v-job-no*/ v-fob space(10) v-ship-phone space(7) carrier.dscr v-frt-terms SKIP
                "<|10><R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
                "<R24><C13><FROM><R26><C13><LINE>" SKIP
                "<R24><C26><FROM><R26><C26><LINE>" SKIP
                "<R24><C39><FROM><R26><C39><LINE>" SKIP
                "<R24><C58><FROM><R26><C58><LINE>" SKIP
                "<R24><C64><FROM><R26><C64><LINE>" SKIP
                "<R24><C71><FROM><R26><C71><LINE>" SKIP
                "<R24><C74><FROM><R26><C74><LINE>" SKIP.
   
   IF lv-bolfmt-int = 1 THEN PUT "<FArial><=5><R+1> Part#/Order#           Release PO#/Job#      Finished Good#              Description                        Units       Qty/Unit     P/C    Weight  <FCourier New>" SKIP(1).
                       ELSE  PUT "<FArial><=5><R+1> Part#                        Release PO#/Job#      Finished Good#              Description                        Units       Qty/Unit    P/C     Weight  <FCourier New>" SKIP(1).
            .

            v-printline = v-printline + 16.
