 /* oe/rep/bolxinl2.i  */  
 
   

   put 
         "<FArial>"  SKIP
         "<P10><ADJUST=LPI><C5><B>STRAIGHT BILL OF LADING"
         "<C35>SHORT FORM - ORIGINAL </B>"
         "<C57>Scehduled Pickup@Ltown______________" SKIP
         "<C35><B>NOT NEGOTIABLE </B> <C57>Schedule Pickup@Gburg______________" SKIP
          "<C1><#1>" /* <R+5><C+25>" /*<IMAGE#1=" ls-full-img1 */ SKIP /* pacific package */             
         "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
         */
          "<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
          "<=1><C3><P10><R+1>" lv-comp-name "<FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)"           
          "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
   /*      "<P10><ADJUST=LPI><=1><R+1>"  */ SKIP
         v-comp-add1 AT 8 SKIP
         v-comp-add2 AT 8  SKIP
         v-comp-add3 AT 8 SKIP
         v-comp-add4 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP(4)
         /*v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
         lv-email AT 8 SKIP(1) */
               "<FCourier New>"
               "Sold To:" SPACE(30) "Ship To:"  SKIP
               SPACE(5) v-comp-name v-ship-name AT 45 skip
               SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
             SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
               SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP
        "<R5><C50><#3>" SKIP
        "<FArial><P14><=#3>" /*<C-20><R-2> <B>Bill Of Lading</B> */ "<P10><ADJUST=LPI>" SKIP
                "<=#3>Customer Dock time_____________________" SKIP
                "<=#3><R+1>Bill of Lading #: " oe-bolh.bol-no  SKIP
                "<=#3><R+2>Page #:             " PAGE-NUM SKIP
                "<=#3><R+3>Date  :                     " oe-bolh.bol-date        SKIP
                "<=#3><R+4><P8>Boxes,Corrugated,NMFC 29275 S2 Class 77 1/2 Quote#__________" SKIP
                "<=#3><R+6><C1><P6><ADJUST=LPI>RECEIVED,subject to the "'"Carrier Rate Agreement"'" or such other contract between "
                    "the Shipper and Carrier in effect on the date of shipment,the property described below, in apparent good order,"
                    "except as noted " SKIP
                    "(contents and condition of contents of package unknown), marked,consigned,and destined as shown below. "
                    "This Bill of Lading is not subject to any tariff or classifications whether individually determined or "
                    "filed with " SKIP
                    "any federal or state regulatory agency, except "
                    "as specifically agreed to in writing by the Shipper and the Carrier, or except to the extent this is a rail shipment "
                    "in which case it is subject to all the terms set forth " SKIP
                    "in the Uniform Freight Classification then in effect."                   
                    SKIP    .
       PUT    "<P10><ADJUST=LPI>"
                "<|10><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
                "<R21><C1><FROM><R21><C81><LINE>" SKIP    
                "<R19><C12><FROM><R23><C12><LINE>" SKIP
              /*  "<R19><C25><FROM><R23><C25><LINE>" SKIP      */
                "<R19><C30><FROM><R23><C30><LINE>" SKIP 
                "<R19><C46><FROM><R23><C46><LINE>" SKIP
                "<R19><C66><FROM><R23><C66><LINE>" SKIP
                /*"<FArial><=4><R+1>    Date                    PO#                               JOB#                 FOB                  Carrier                                                 Freight Terms" SKIP */
                "<FArial><=4><R+1><P10><ADJUST=LPI>    Date                    FOB                                        Trailer#                                 Carrier                                            Freight Terms" SKIP 
                "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) /*v-po-no FORM "x(15)" space(2) v-job-no*/ v-fob space(10) v-trailer SPACE(12) carrier.dscr FORM "x(26)" v-frt-terms SKIP
                "<|10><R24><C1><#5><FROM><R26><C81><RECT>" SKIP
                "<R24><C13><FROM><R26><C13><LINE>" SKIP
                "<R24><C28><FROM><R26><C28><LINE>" SKIP
                "<R24><C37><FROM><R26><C37><LINE>" SKIP
                "<R24><C56><FROM><R26><C56><LINE>" SKIP  
                "<R24><C68><FROM><R26><C68><LINE>" SKIP
                "<R24><C76><FROM><R26><C76><LINE>" SKIP  
           "<FArial><=5><C+69>  Partial"                                           
           "<FArial><=5><R+1> Part/Item#                PO#                                   JOB#           Description                               Unit-Quantity             Complete  Weight" SKIP
            "<FCourier New>"                                  
            .

            v-printline = v-printline + 16.
