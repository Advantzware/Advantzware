 /* oe/rep/bolprotg2.i  */  
 
 
   put "<FArial>"  SKIP
          "<P14><C+40><B>Bill Of Lading</B> " SKIP
          "<C1><#3><R+10><C+25><IMAGE#1=" ls-full-img1  
          /*"<C1><#1><R+5><C+25>" <IMAGE#1=" ls-full-img1  SKIP /* pacific package */  */           
        /* "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
         */
          /*"<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
          "<=1><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
          "<P10></B>"*/

          "<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
         "<P10><=1><R+4>"  
         v-comp-add1 AT 8 SKIP
         v-comp-add2 AT 8  SKIP
         v-comp-add3 AT 8 SKIP
         "Phone : 416(293) 9500" AT 8 SKIP
         "Fax     : 416(293) 9600" AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
         "www.protagon.com" AT 8 SKIP(1)
               "<FCourier New>"
               "Sold To:" SPACE(30) "Ship To:"  SKIP
               SPACE(5) v-comp-name v-ship-name AT 45 skip
               SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
             SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
               SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP
        "<R5><C50><#3>" SKIP
        "<FArial><P14><=#3>" /*<C-20><R-2> <B>Bill Of Lading</B> */ "<P10>" SKIP
                "<=#3><R+1><B>BOL #: " oe-bolh.bol-no "</B>" SKIP(1)
                "<=#3><R+3>Date: " oe-bolh.bol-date        SKIP
                "<=#3><R+4>Contact: " v-shipto-contact SKIP
                "<=#3><R+5>Phone: " v-phone FORM "x(15)"  SKIP     
                "<|10><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
                "<R21><C1><FROM><R21><C81><LINE>" SKIP    
                "<R19><C12><FROM><R23><C12><LINE>" SKIP
              /*  "<R19><C25><FROM><R23><C25><LINE>" SKIP      
                "<R19><C34><FROM><R23><C34><LINE>" SKIP */
                "<R19><C30><FROM><R23><C30><LINE>" SKIP
                "<R19><C46><FROM><R23><C46><LINE>" SKIP
                "<R19><C66><FROM><R23><C66><LINE>" SKIP
                /*"<FArial><=4><R+1>    Date                    PO#                               JOB#                 FOB                  Carrier                                                 Freight Terms" SKIP */
/*                 "<FArial><=4><R+1>    Date                    FOB                                        Phone                                 Carrier                                          Freight Terms" SKIP */
/*                 "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) /*v-po-no FORM "x(15)" space(2) v-job-no*/ v-fob space(10) v-ship-phone space(7) carrier.dscr v-frt-terms SKIP                                 */
                "<FArial><=4><R+1><C2>Date<C13>FOB<C31>Phone<C47>Carrier<C67>Freight Terms" SKIP 
                "<FCourier New><=4><R+3><C2>" oe-bolh.bol-date 
                "<C13>" v-fob 
                "<C31>" v-ship-phone
                "<C47>" carrier.dscr 
                "<C67>" v-frt-terms SKIP
                "<|10><R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
                "<R24><C13><FROM><R26><C13><LINE>" SKIP
                "<R24><C26><FROM><R26><C26><LINE>" SKIP
                "<R24><C39><FROM><R26><C39><LINE>" SKIP
                "<R24><C58><FROM><R26><C58><LINE>" SKIP
                "<R24><C64><FROM><R26><C64><LINE>" SKIP
                "<R24><C71><FROM><R26><C71><LINE>" SKIP
                "<R24><C74><FROM><R26><C74><LINE>" SKIP.
   
   IF lv-bolfmt-int = 1 THEN PUT "<FArial><=5><R+1> Part#/Order#                       PO#                  Finished Good#              Description                         Units       Qty/Unit   P/C    Weight  <FCourier New>" SKIP(1).
                       ELSE  PUT "<FArial><=5><R+1> Part#                        PO#                            Finished Good#        Description                                Units       Qty/Unit   P/C    Weight  <FCourier New>" SKIP(1).
            .

            v-printline = v-printline + 16.

