 /* oe/rep/bolmsbx2.i  */  

   PUT {1}
         "<FArial>"  SKIP
          "<P14><C+40><B>Bill Of Lading</B> " SKIP
          "<C5><R3><#1><R+7><C+75><IMAGE#1=" ls-full-img1  SKIP /* pacific package */             
        /* "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
         */
       /*
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
         lv-email AT 8 */
             "<=1><R+7>" 
             "<FCourier New><B><P12>                          PHONE (216)281-3980" SKIP
             "                              FAX   (216)281-5707<P10>" SKIP(1)
             "<FCourier New><P10>"
            "    Sold To:" SPACE(40) "Ship To:"  SKIP
             SPACE(9) v-comp-name v-ship-name AT 56 skip
             SPACE(9) v-comp-addr[1] v-ship-addr[1] AT 56 SKIP
           SPACE(9) v-comp-addr[2] v-ship-addr[2] AT 56 SKIP
             SPACE(9) v-comp-addr3 v-ship-addr3 AT 56 SKIP
       SPACE(9) v-phone-num SKIP
        "<R5><C50><#3>" SKIP
        "<FArial><P14><=#3>" /*<C-20><R-2> <B>Bill Of Lading</B> */ "<P10>" SKIP
                "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP
                "<=#3><R+1><B>Page #: " 
                   string(trim(string(PAGE-NUMBER - v-last-page,">9"))
                   + " of " +
                    trim(string(v-page-tot,">9")))
                           format "x(8)" "</B>" SKIP
                "<=#3><R+2>Date: " oe-bolh.bol-date        SKIP
                "<=#3><R+3>" /*Ship Date:" oe-bolh.ship-date        */ SKIP
                 SKIP     
                "<|4><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
                "<R21><C1><FROM><R21><C81><LINE>" SKIP    
                "<R19><C12><FROM><R23><C12><LINE>" SKIP
              /*  "<R19><C25><FROM><R23><C25><LINE>" SKIP      
                "<R19><C34><FROM><R23><C34><LINE>" SKIP */
                "<R19><C35><FROM><R23><C35><LINE>" SKIP
                "<R19><C66><FROM><R23><C66><LINE>" SKIP
                /*"<FArial><=4><R+1>    Date                    PO#                               JOB#                 FOB                  Carrier                                                 Freight Terms" SKIP */
                "<FArial><=4><R+1>    Date                    FOB                                                                                   Carrier                                            Freight Terms" SKIP 
                "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) /*v-po-no FORM "x(15)" space(2) v-job-no*/ v-fob space(15) carrier.dscr FORM "x(33)" SPACE(4) v-frt-terms SKIP
                "<|4><R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
                "<R24><C13><FROM><R26><C13><LINE>" SKIP
                "<R24><C26><FROM><R26><C26><LINE>" SKIP
                "<R24><C39><FROM><R26><C39><LINE>" SKIP
                "<R24><C56><FROM><R26><C56><LINE>" SKIP  
                "<R24><C65><FROM><R26><C65><LINE>" SKIP
                "<R24><C76><FROM><R26><C76><LINE>" SKIP            
            "<FArial><=5><R+1> Part#                        PO#                            Finished Good#        Our Order#                    Unit-Quantity Partial/Complete  Weight" SKIP(1)
            "<FCourier New>"                                  
            .

    v-printline = v-printline + 16.

