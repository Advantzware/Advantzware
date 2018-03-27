 /* oe/rep/bolmcel2.i  */  
 
 
   put "<FArial>"  SKIP
          "<P14><C+40><B>Bill Of Lading</B> " SKIP
          "<C1><#1><R+5><C+25>" /*<IMAGE#1=" ls-full-img1 */ SKIP /* pacific package */             
        /* "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
         */
          "<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
          "<=1><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
          "<P10></B>"

          "<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
         "<P10><=1><R+3>"  
         v-comp-add1 AT 8 SKIP
         v-comp-add2 AT 8  SKIP
         v-comp-add3 AT 8 SKIP
         v-comp-add4 AT 8 SKIP
         v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
         lv-email AT 8 SKIP(1)
               "<FCourier New>"
               SPACE(38) "Ship To:"  SKIP
               v-ship-name AT 45 skip
               v-ship-addr[1] AT 45 SKIP
               v-ship-addr[2] AT 45 SKIP
               v-ship-addr3 AT 45 SKIP
        "<R5><C50><#3>" SKIP
        "<FArial><P14><=#3>" /*<C-20><R-2> <B>Bill Of Lading</B> */ "<P10>" SKIP
                "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP(1)
                "<=#3><R+2>Date: " oe-bolh.bol-date        SKIP
                "<=#3><R+3>Contact: " v-shipto-contact SKIP
                "<=#3><R+4>Phone: " v-phone FORM "x(15)"  SKIP     
                "<|10><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
                "<R21><C1><FROM><R21><C81><LINE>" SKIP    
                "<R19><C12><FROM><R23><C12><LINE>" SKIP
              /*  "<R19><C25><FROM><R23><C25><LINE>" SKIP      
                "<R19><C34><FROM><R23><C34><LINE>" SKIP */
                "<R19><C30><FROM><R23><C30><LINE>" SKIP
                "<R19><C48><FROM><R23><C48><LINE>" SKIP
                "<R19><C66><FROM><R23><C66><LINE>" SKIP
                /*"<FArial><=4><R+1>    Date                    PO#                               JOB#                 FOB                  Carrier                                                 Freight Terms" SKIP */
                "<FArial><=4><R+1>    Date                    FOB                                        PRO #                                    Carrier                                          Freight Terms" SKIP 
                "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) /*v-po-no FORM "x(15)" space(2) v-job-no*/ v-fob space(10) oe-bolh.trailer SPACE carrier.dscr v-frt-terms SKIP
                "<|10><R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
                "<R24><C13><FROM><R26><C13><LINE>" SKIP
                "<R24><C31><FROM><R26><C31><LINE>" SKIP
                "<R24><C49><FROM><R26><C49><LINE>" SKIP
                "<R24><C58><FROM><R26><C58><LINE>" SKIP
                "<R24><C64><FROM><R26><C64><LINE>" SKIP
                "<R24><C71><FROM><R26><C71><LINE>" SKIP
                "<R24><C74><FROM><R26><C74><LINE>" SKIP.
   
/*    IF lv-bolfmt-int = 1 THEN */
       PUT "<FArial><=5><R+1> Order#'s"
       "<C14>PO#"
       "<C32>Part #"
       "<C50>Class"
       "<C59>Units"
       "<C65>Qty/Unit    P/C"
       "<C75>Weight"
       "<FCourier New>" SKIP(1).
/*    ELSE                                                                                                                                                                                                                  */
/*        PUT "<FArial><=5><R+1> Order#/Job#                  Your PO#                       Item Part#                                                       Units       Qty/Unit   P/C    Weight <FCourier New>" SKIP(1). */
/*             .                                                                                                                                                                                                            */

            v-printline = v-printline + 16.
