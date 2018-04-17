 /* oe/rep/bolsoule2.i  */  
 
   PUT "<FArial>"  SKIP
          "<P14><C+40><B>Bill Of Lading</B> " SKIP.


    PUT "<C1><R2><#1><R+9><C+40><IMAGE#1=" ls-full-img1 SKIP
        "<FArial>" 
/*         "Email: " + trim(lv-email) AT 15 SKIP(1) */

/*     PUT "<=1>" SKIP(1). */
        
/*         SKIP (7) */

/*           "<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"                                                */
/*           "<=1><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)"  */
/*           "<P10></B>"                                                                                  */
   
/*          "<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)" */
         "<P10><=1><R+7>"
/*          v-comp-add1 AT 8 SKIP                                                        */
/*          v-comp-add2 AT 8  SKIP                                                       */
/*          v-comp-add3 AT 8 SKIP                                                        */
/*          v-comp-add4 AT 8 SKIP                                                        */
/*          v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP  */
               "<FCourier New><P10>"
              "Email: " + trim(lv-email) FORMAT "x(56)" AT 15 SKIP(1)
               "Sold To:" SPACE(30) "Ship To:"  SKIP
               SPACE(5) v-comp-name v-ship-name AT 45 skip
               SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
             SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
               SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP

        "<R5><C50><#3>" SKIP
        "<FArial><P14><=#3>" /*<C-20><R-2> <B>Bill Of Lading</B> */ "<P10>" SKIP
                "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP
/*                 "<=#3><R+1>Order#: " lv-ord-no "</B>" */
                "<=#3><R+2>Date: " oe-bolh.bol-date        SKIP
                "<=#3><R+3>Contact: " v-shipto-contact SKIP
                "<=#3><R+4>Phone: " v-phone FORM "x(15)"  SKIP     
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
                "<R24><C5><FROM><R26><C5><LINE>" SKIP
                "<R24><C18><FROM><R26><C18><LINE>" SKIP
                /*"<R24><C26><FROM><R26><C26><LINE>" SKIP*/
                "<R24><C31><FROM><R26><C31><LINE>" SKIP
                "<R24><C58><FROM><R26><C58><LINE>" SKIP
                "<R24><C64><FROM><R26><C64><LINE>" SKIP
                "<R24><C71><FROM><R26><C71><LINE>" SKIP
                "<R24><C74><FROM><R26><C74><LINE>" SKIP.
   
   IF lv-bolfmt-int = 1 THEN PUT "<FArial><=5><R+1> Line#   Part#/FG#/Ord#               PO#                             Description / Components                      Units       Qty/Unit   P/C    Weight  <FCourier New>" SKIP(1).
                       ELSE  PUT "<FArial><=5><R+1> Line#  Part# / FG#                          PO#                         Description / Components                      Units       Qty/Unit   P/C    Weight  <FCourier New>" SKIP(1).
            .

            v-printline = v-printline + 16.
