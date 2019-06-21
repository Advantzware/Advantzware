 /* oe/rep/bolprm2.i for Premier  */  
 
   

   put 
         "<FArial>"  SKIP
         "<#1><P10><ADJUST=LPI><C50><B>STRAIGHT BILL OF LADING"  SKIP
         "<=1><R+1><C25></B>"            SKIP
         "<=1><R+2><C25><B><C50>Bill of Lading #: " oe-bolh.bol-no   "</B>"
         "<R-4><C1><#2><R+10><C+60><IMAGE#1=" ls-full-img1 SKIP              
         /*"<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
         */
      /*    "<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
          "<=1><C3><P10><R+1>" lv-comp-name "<FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)"           
          "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
     */
   /*      "<P10><ADJUST=LPI><=1><R+1>"  */ 
/*         "<=2><R+4>"SKIP
         v-comp-add1 AT 37 SKIP
         v-comp-add2 AT 37  SKIP
         v-comp-add3 AT 37 SKIP
         v-comp-add4 AT 37 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP(1)
*/         
         /*v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
         lv-email AT 8 SKIP(1) */
               "<FCourier New>"
               "Bill To:" SPACE(30) "Ship To:"  SKIP
               SPACE(5) v-comp-name v-ship-name AT 45 skip
               SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
             SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
               SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP 
        "<R5><C50><#3>" SKIP
        "<FArial><P14><=#3>" /*<C-20><R-2> <B>Bill Of Lading</B> */ "<P10><ADJUST=LPI>" SKIP
                "<=#3>Customer Dock time_____________________" SKIP
                 SKIP              
                "<=#3><R+2>Page #:             " PAGE-NUM SKIP
                "<=#3><R+3>Date  :                     " oe-bolh.bol-date        SKIP
                /*"<=#3><R+4><P8>Boxes,Corrugated,NMFC 29275 S2 Class 77 1/2 Quote#__________" */ SKIP
                SKIP(3).

       PUT    "<P10><ADJUST=LPI>"
                "<|5><R17><C1><#4><FROM><R21><C81><RECT>" SKIP
                "<R19><C1><FROM><R19><C81><LINE>" SKIP    
                "<R17><C12><FROM><R21><C12><LINE>" SKIP
              /*  "<R19><C25><FROM><R23><C25><LINE>" SKIP      */
                "<R17><C30><FROM><R21><C30><LINE>" SKIP 
                "<R17><C46><FROM><R21><C46><LINE>" SKIP
                "<R17><C67><FROM><R21><C67><LINE>" SKIP
                "<FArial><=4><R+1><P10><ADJUST=LPI>          Date                              FOB                                Trailer#                                 Carrier                                   Freight Terms" SKIP 
                "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) /*v-po-no FORM "x(15)" space(2) v-job-no*/ v-fob space(10) v-trailer SPACE(11) carrier.dscr v-frt-terms SKIP
                "<|5><R22><C1><#5><FROM><R24><C81><RECT>" SKIP    
                "<R22><C13><FROM><R24><C13><LINE>" SKIP
                "<R22><C28><FROM><R24><C28><LINE>" SKIP
                "<R22><C37><FROM><R24><C37><LINE>" SKIP
                "<R22><C56><FROM><R24><C56><LINE>" SKIP  
                "<R22><C68><FROM><R24><C68><LINE>" SKIP
                /*"<R24><C76><FROM><R26><C76><LINE>" SKIP  */
           "<FArial><=5><C18>Order#/ " /*C72>Partial"     */
           "<FArial><=5><R+1>    Item# / Part                Customer PO#              JOB#                       Description                     Unit-Quantity                 Weight  " SKIP(1)
            "<FCourier New>"                                  
            .

            v-printline = v-printline + 16.
